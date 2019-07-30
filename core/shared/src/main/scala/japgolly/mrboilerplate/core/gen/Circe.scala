package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.data._
import japgolly.mrboilerplate.core.StringUtils._
import monocle.macros.Lenses

object Circe extends Generator {

  override val title = "Circe"

  @Lenses
  final case class Options(singlesAsObjects: Boolean,
                           monadicObjects: Boolean,
                           keyConstants: Boolean)

  override def gen(opt: Options, glopt: GlobalOptions): TypeDef => List[String] = {
    case c: Cls        => genCls(c, opt, glopt)
    case s: SealedBase => genSB(s, opt, glopt)
  }

  private def genCls(cls: Cls, opt: Options, glopt: GlobalOptions): List[String] = {
    import cls._

    val suffix = termSuffix(glopt)

    val apply =
      name + ".apply"

    val unapply =
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

    val fieldNameStrsOrKeys =
      if (opt.keyConstants)
        fields.map(f => mkKey(f.name.value)).mkString(", ")
      else
        fieldNameStrs

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
          val d = s"Decoder.forProduct${fields.size}($fieldNameStrsOrKeys)($apply)"
          val e = s"Encoder.forProduct${fields.size}($fieldNameStrsOrKeys)($unapply)"
          (d, e)
      }

    val decoder =
      s"""
         |implicit $valDef decoder$suffix${typeParamDefsAndEvTC("Decoder")}: Decoder[$nameWithTypesApplied] =
         |  $decoderBody
         |""".stripMargin.trim

    val encoder =
      s"""
         |implicit $valDef encoder$suffix${typeParamDefsAndEvTC("Encoder")}: Encoder[$nameWithTypesApplied] =
         |  $encoderBody
         |""".stripMargin.trim

    if (opt.keyConstants) {
      val keys = fields.iterator.map(f => s"private final val ${mkKey(f.name.pad)} = ${f.name.quote}").mkString("\n")
      keys :: decoder :: encoder :: Nil
    } else
      decoder :: encoder :: Nil
  }

  private def genSB(sb: SealedBase, opt: Options, glopt: GlobalOptions): List[String] = {
    Nil
  }

}