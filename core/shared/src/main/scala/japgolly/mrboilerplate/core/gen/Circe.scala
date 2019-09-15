package japgolly.mrboilerplate.core.gen

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.mrboilerplate.core.MaxLen
import japgolly.mrboilerplate.core.data._
import japgolly.mrboilerplate.core.StringUtils._
import monocle.macros.Lenses
import japgolly.univeq._

object Circe extends Generator {

  override val title = "Circe"

  @Lenses
  final case class Options(objectCodecs: Boolean,
                           singlesAsObjects: Boolean,
                           monadicObjects: Boolean,
                           keyConstants: Boolean,
                           sumTypes: Options.SumTypeFormat)

  object Options {
    sealed trait SumTypeFormat
    object SumTypeFormat {
      case object TypeToValue extends SumTypeFormat
      case object UntaggedUnion extends SumTypeFormat
      implicit def univEq: UnivEq[SumTypeFormat] = UnivEq.derive
      val values = AdtMacros.adtValues[SumTypeFormat]
    }
  }

  import Options.SumTypeFormat

  override def gen(opt: Options)(implicit glopt: GlobalOptions): TypeDef => List[String] = {
    case c: Cls        => genCls(c, opt)
    case o: Obj        => genObj(o, opt)
    case s: SealedBase => genSB(s, opt)
  }

  // ===================================================================================================================

  private def genObj(obj: Obj, opt: Options)(implicit glopt: GlobalOptions): List[String] = {
    import obj._

    if (opt.objectCodecs) {
      val decoderBody = s"Decoder.const($name)"
      val encoderBody = "Encoder.encodeUnit.contramap(_ => ())"

      val (decoderDecl, encoderDecl) = mkDef(obj)

      val decoder = s"$decoderDecl =\n  $decoderBody"
      val encoder = s"$encoderDecl =\n  $encoderBody"

      decoder :: encoder :: Nil
    } else
      Nil
  }

  // ===================================================================================================================

  private def genCls(cls: Cls, opt: Options)(implicit glopt: GlobalOptions): List[String] = {
    import cls._

    def mkKey(f: String) =
      "CirceKey" + instanceNameSuffix + f.withHeadUpper

    def quoteOrKey(f: FieldName) =
      if (opt.keyConstants) mkKey(f.value) else f.quote

    def quoteOrKeyPad(f: FieldName) =
      if (opt.keyConstants) mkKey(f.pad) else f.quotePad

    val (decoderBody, encoderBody) =
      fieldCount match {
        case 0 =>
          val d = s"Decoder.const($name())"
          val e = "Encoder.encodeUnit.contramap(_ => ())"
          (d, e)

        case 1 if !opt.singlesAsObjects =>
          val d = s"Decoder[${fields.head.typ}].map($apply)"
          val e = s"Encoder[${fields.head.typ}].contramap($unapply)"
          (d, e)

        case _ if opt.monadicObjects =>
          val gets = fields.map(f => s"      ${f.name.pad} <- c.get[${f.typ}](${quoteOrKey(f.name)})")
          val d =
            s"""
               |Decoder.instance { c =>
               |    for {
               |${gets.mkString("\n")}
               |    } yield $name($fieldNames)
               |  }
               |""".stripMargin.trim
          val sets = fields.map(f => s"    ${quoteOrKeyPad(f.name)} -> value.${f.name}.asJson,")
          val e =
            s"""
               |Encoder.instance(value => Json.obj(
               |${sets.mkString("\n")}
               |  ))
               |""".stripMargin.trim
          (d, e)

        case _ =>
          val fieldNameStrsOrKeys =
            if (opt.keyConstants)
              fields.map(f => mkKey(f.name.value)).mkString(", ")
            else
              fieldNameStrs
          val d = s"Decoder.forProduct${fields.size}($fieldNameStrsOrKeys)($apply)"
          val e = s"Encoder.forProduct${fields.size}($fieldNameStrsOrKeys)($unapply)"
          (d, e)
      }

    val (decoderDecl, encoderDecl) = mkDef(cls)

    val decoder = s"$decoderDecl =\n  $decoderBody"
    val encoder = s"$encoderDecl =\n  $encoderBody"

    if (opt.keyConstants) {
      val keys = fields.iterator.map(f => s"private final val ${mkKey(f.name.pad)} = ${f.name.quote}").mkString("\n")
      keys :: decoder :: encoder :: Nil
    } else
      decoder :: encoder :: Nil
  }

  // ===================================================================================================================

  private def genSB(sb: SealedBase, opt: Options)(implicit glopt: GlobalOptions): List[String] = {
    import sb._
    import sb.concreteTransitiveChildren._

    if (children.isEmpty)
      return Nil

    def keyFor(t: TypeDef.Concrete) =
      maxTailSuffixLen.pad2("\"" + t.tailSuffix.withHeadLower + "\"")

    val maxEncCase =
      MaxLen.derive(children.map {
        case c: Cls => "a: " + c.typeNamePoly
        case o: Obj => if (opt.objectCodecs) "a@ " + o.name else o.name
      })

    val (decoderBody, encoderBody) =
      opt.sumTypes match {

        case SumTypeFormat.TypeToValue =>
          def decCase(t: TypeDef.Concrete) = t match {
            case o: Obj if !opt.objectCodecs => s"  case (${keyFor(o)}, _) => Right(${o.name})"
            case c                           => s"  case (${keyFor(c)}, c) => c.as[${c.typeNamePoly}]"
          }
          val d =
            s"""| decodeSumBySoleKey {
                |${children.map(decCase).mkString("\n")}
                |}""".stripMargin
          def encCase(t: TypeDef.Concrete) = t match {
            case c: Cls =>
              s"  case ${maxEncCase.pad("a: " + c.typeNamePoly)} => Json.obj(${keyFor(c)} -> a.asJson)"
            case o: Obj =>
              if (opt.objectCodecs)
                s"  case ${maxEncCase.pad("a@ " + o.name)} => Json.obj(${keyFor(o)} -> a.asJson)"
              else
                s"  case ${maxEncCase.pad(o.name)} => Json.obj(${keyFor(o)} -> ().asJson)"
          }
          val e =
            s"""| Encoder.instance {
                |${children.map(encCase).mkString("\n")}
                |}""".stripMargin
          (d, e)

        case SumTypeFormat.UntaggedUnion =>
          def decCase(t: TypeDef.Concrete) = s"\n  Decoder[${maxTypeNameLen.pad(t.typeName)}].widen[$name]"
          val d = children.map(decCase).mkString(" or")
          def encCase(t: TypeDef.Concrete) = t match {
            case c: Cls =>
              s"  case ${maxEncCase.pad("a: " + c.typeNamePoly)} => a.asJson"
            case o: Obj =>
              if (opt.objectCodecs)
                s"  case ${maxEncCase.pad("a@ " + o.name)} => a.asJson"
              else
                s"  case ${maxEncCase.pad(o.name)} => ().asJson"
          }
          val e =
            s"""| Encoder.instance {
                |${children.map(encCase).mkString("\n")}
                |}""".stripMargin
          (d, e)
      }

    val (decoderDecl, encoderDecl) = mkDef(sb)

    val decoder = s"$decoderDecl =$decoderBody"
    val encoder = s"$encoderDecl =$encoderBody"

    decoder :: encoder :: Nil
  }

  // ===================================================================================================================

  private def mkDef(td: TypeDef)(implicit g: GlobalOptions): (String, String) = {
    import td._
    val d = s"implicit $valDef decoder$instanceNameSuffix${typeParamDefsAndEvTC("Decoder")}: Decoder[$typeNamePoly]"
    val e = s"implicit $valDef encoder$instanceNameSuffix${typeParamDefsAndEvTC("Encoder")}: Encoder[$typeNamePoly]"
    (d, e)
  }

  override def initStatements(data: Iterable[TypeDef], opt: Options)(implicit glopt: GlobalOptions) = {
    val sumTypeExists = data.exists {
      case s: SealedBase       => s.concreteTransitiveChildren.children.nonEmpty
      case _: TypeDef.Concrete => false
    }

    val imports = collection.mutable.ArrayBuffer.empty[String]
    imports += "import io.circe._"
    imports += "import io.circe.syntax._"

    var results = List.empty[String]

    if (sumTypeExists)
      opt.sumTypes match {
        case SumTypeFormat.TypeToValue   => results ::= decodeSumBySoleKey
        case SumTypeFormat.UntaggedUnion => imports += importCatsFunctor
      }

    imports.sorted.mkString("\n") :: results
  }

  private val importCatsFunctor =
    "import cats.syntax.functor._"

  private val decodeSumBySoleKey =
    """
      |def decodeSumBySoleKey[A](f: PartialFunction[(String, ACursor), Decoder.Result[A]]): Decoder[A] = {
      |  def keyErr = "Expected a single key indicating the subtype"
      |  Decoder.instance { c =>
      |    c.keys match {
      |      case Some(it) =>
      |        it.toList match {
      |          case singleKey :: Nil =>
      |            val arg  = (singleKey, c.downField(singleKey))
      |            def fail = Left(DecodingFailure("Unknown subtype: " + singleKey, c.history))
      |            f.applyOrElse(arg, (_: (String, ACursor)) => fail)
      |          case Nil  => Left(DecodingFailure(keyErr, c.history))
      |          case keys => Left(DecodingFailure(s"$keyErr, found multiple: $keys", c.history))
      |        }
      |      case None => Left(DecodingFailure(keyErr, c.history))
      |    }
      |  }
      |}
    """.stripMargin.trim
}