package japgolly.mrboilerplate.core.gen

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.mrboilerplate.core.data._
import japgolly.mrboilerplate.core.StringUtils._
import japgolly.mrboilerplate.core.gen.Circe.Options.SumTypeFormat
import monocle.macros.Lenses
import japgolly.univeq._

object Circe extends Generator {

  override val title = "Circe"

  @Lenses
  final case class Options(singlesAsObjects: Boolean,
                           monadicObjects: Boolean,
                           keyConstants: Boolean,
                           sumTypes: Options.SumTypeFormat,
                          )

  object Options {
    sealed trait SumTypeFormat
    object SumTypeFormat {
      case object TypeToValue extends SumTypeFormat
      case object UntaggedUnion extends SumTypeFormat
      implicit def univEq: UnivEq[SumTypeFormat] = UnivEq.derive
      val values = AdtMacros.adtValues[SumTypeFormat]
    }
  }

  override def gen(opt: Options)(implicit glopt: GlobalOptions): TypeDef => List[String] = {
    case c: Cls        => genCls(c, opt)
    case s: SealedBase => genSB(s, opt)
  }

  // ===================================================================================================================

  private def genCls(cls: Cls, opt: Options)(implicit glopt: GlobalOptions): List[String] = {
    import cls._

    def apply = s"$name.apply$typeParamAp"

    def unapply =
      fields match {
        case f :: Nil => "_." + f.name
        case fs       => fs.map("a." + _.name).mkString("a => (", ", ", ")")
      }

    def mkKey(f: String) =
      "CirceKey" + suffix + f.withHeadUpper

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

    val (decoderDecl, encoderDecl) = mkDecls(cls, decoderBody, encoderBody)

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
    import sb.{nonAbstractTransitiveChildrenMaxLen => clsMaxLen, _}

    if (nonAbstractTransitiveChildren.isEmpty)
      return Nil

    def keyFor(c: Cls) = clsMaxLen.pad2("\"" + c.name.withHeadLower + "\"")

    val (decoderBody, encoderBody) =
      opt.sumTypes match {

        case SumTypeFormat.TypeToValue =>
          def decCase(c: Cls) = s"  case (${keyFor(c)}, c) => c.as[${c.name}]"
          val d =
            s"""| decodeSumBySoleKey {
                |${nonAbstractTransitiveChildren.map(decCase).mkString("\n")}
                |}""".stripMargin
          def encCase(c: Cls) = s"  case a: ${clsMaxLen.pad(c.name)} => Json.obj(${keyFor(c)} -> a.asJson)"
          val e =
            s"""| Encoder.instance {
                |${nonAbstractTransitiveChildren.map(encCase).mkString("\n")}
                |}""".stripMargin
          (d, e)

        case SumTypeFormat.UntaggedUnion =>
          def decCase(c: Cls) = s"\n  Decoder[${clsMaxLen.pad(c.name)}].widen[$name]"
          val d = nonAbstractTransitiveChildren.map(decCase).mkString(" or")
          def encCase(c: Cls) = s"  case a: ${clsMaxLen.pad(c.name)} => a.asJson"
          val e =
            s"""| Encoder.instance {
                |${nonAbstractTransitiveChildren.map(encCase).mkString("\n")}
                |}""".stripMargin
          (d, e)
      }

    val (decoderDecl, encoderDecl) = mkDecls(sb, decoderBody, encoderBody)

    val decoder = s"$decoderDecl =$decoderBody"
    val encoder = s"$encoderDecl =$encoderBody"

    decoder :: encoder :: Nil
  }

  // ===================================================================================================================

  private def mkDecls(td: TypeDef, decoderBody: String, encoderBody: String)
                     (implicit g: GlobalOptions): (String, String) = {
    import td._
    val d = s"implicit $valDef decoder$suffix${typeParamDefsAndEvTC("Decoder")}: Decoder[$nameWithTypesApplied]"
    val e = s"implicit $valDef encoder$suffix${typeParamDefsAndEvTC("Encoder")}: Encoder[$nameWithTypesApplied]"
    (d, e)
  }

  override def initStatements(data: Traversable[TypeDef], opt: Options)(implicit glopt: GlobalOptions) = {
    val sumTypeExists = data.exists {
      case s: SealedBase => s.nonAbstractTransitiveChildren.nonEmpty
      case _: Cls => false
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