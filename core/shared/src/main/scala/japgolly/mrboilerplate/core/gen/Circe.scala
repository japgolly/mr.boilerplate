package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.Cls
import japgolly.mrboilerplate.core.StringUtils._
import monocle.macros.Lenses

object Circe extends Generator {

  override val title = "Circe"

  @Lenses
  final case class Options(singlesAsObjects: Boolean,
                           monadicObjects: Boolean)

  override val defaultOptions = Options(
    singlesAsObjects = true,
    monadicObjects   = false)

  override def generate(cls: Cls, opt: Options, glopt: GlobalOptions): List[String] = {
    import cls._

    val suffix =
      if (glopt.shortInstanceNames)
        ""
      else
        name.withHeadUpper

    val apply =
      name + ".apply"

    val unapply =
      fields match {
        case f :: Nil => "_." + f.name
        case fs => fs.map("a." + _.name).mkString("a => (", ", ", ")")
      }

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
          val gets = fields.map(f => s"      ${f.name.pad} <- c.get[${f.typ}](${f.name.quote})")
          val d =
            s"""
               |Decoder.instance { c =>
               |    for {
               |${gets.mkString("\n")}
               |    } yield $name($fieldNames)
               |  }
               |""".stripMargin.trim
          val sets = fields.map(f => s"      ${f.name.quotePad} -> value.${f.name}.asJson,")
          val e =
            s"""
               |Encoder.instance { value =>
               |    Json.obj(
               |${sets.mkString("\n")}
               |    )
               |  }
               |""".stripMargin.trim
          (d, e)

        case _ =>
          val d = s"Decoder.forProduct${fields.size}($fieldNameStrs)($apply)"
          val e = s"Encoder.forProduct${fields.size}($fieldNameStrs)($unapply)"
          (d, e)
      }

    val decoder =
      s"""
         |implicit $valDef decoder$suffix$typeParamDefs: Decoder[$nameWithTypesApplied] =
         |  $decoderBody
         |""".stripMargin.trim

    val encoder =
      s"""
         |implicit $valDef encoder$suffix$typeParamDefs: Encoder[$nameWithTypesApplied] =
         |  $encoderBody
         |""".stripMargin.trim

    decoder :: encoder :: Nil

  }
}