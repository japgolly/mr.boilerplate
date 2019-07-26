package japgolly.mrboilerplate.core

object Circe extends Generator {
  import StringUtils._

  final case class Options(shortInstanceNames: Boolean,
                           singlesAsObjects: Boolean,
                           monadicObjects: Boolean)

  override val defaultOptions = Options(
    shortInstanceNames = false,
    singlesAsObjects   = true,
    monadicObjects     = false)

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
            val d = s"Decoder.const($name())"
            val e = "Encoder.encodeUnit.contramap(_ => ())"
            (d, e)

          case 1 if !options.singlesAsObjects =>
            val d = s"Decoder[${fields.head.typ}].map($apply)"
            val e = s"Encoder[${fields.head.typ}].contramap($unapply)"
            (d, e)

          case _ if options.monadicObjects =>
            val gets = fields.map(f => s"      ${f.name} <- c.get[${f.typ}](${f.name.quote})")
            val d =
              s"""
                 |Decoder.instance { c =>
                 |    for {
                 |${gets.mkString("\n")}
                 |    } yield $name($fieldNames)
                 |  }
                 |""".stripMargin.trim
            val sets = fields.map(f => s"      ${f.name.quote} -> value.${f.name}.asJson,")
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