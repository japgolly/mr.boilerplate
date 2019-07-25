package japgolly.mrboilerplate.core

object Circe extends Generator {
  import StringUtils._

  final case class Options(shortInstanceNames: Boolean,
                           singlesAsObjects: Boolean)

  override val defaultOptions = Options(
    shortInstanceNames = false,
    singlesAsObjects   = true,
  )

  override val generate =
    options => c => {
      import c._

      val suffix =
        if (options.shortInstanceNames)
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
        c.fieldCount match {
          case 0 =>
            val d = s"Decoder.const(${c.name}())"
            val e = "Encoder.encodeUnit.contramap(_ => ())"
            (d, e)
          case 1 if !options.singlesAsObjects =>
            val d = s"Decoder[${c.fields.head.typ}].map($apply)"
            val e = s"Encoder[${c.fields.head.typ}].contramap($unapply)"
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