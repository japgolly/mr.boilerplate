package japgolly.mrboilerplate.core

import sourcecode.Line
import utest._

object CirceTest extends TestSuite {
  import CoreTestUtil._
  import UnsafeTypes._

  private def assertGen(c: Class, o: Circe.Options = Circe.defaultOptions)(expect: String*)
                       (implicit l: Line): Unit = {
    val actual = Circe.generate(o)(c)
    assertSeq(actual, expect.map(_.trim))
  }

  override def tests = Tests {

    'mono0 - assertGen(
      Class("Mono", Nil, Nil)
    )(
      """
        |implicit val decoderMono: Decoder[Mono] =
        |  Decoder.const(Mono())
        |""".stripMargin,
      """
        |implicit val encoderMono: Encoder[Mono] =
        |  Encoder.encodeUnit.contramap(_ => ())
        |""".stripMargin)

    'mono1 - assertGen(
      Class("FieldName", Nil, List("value" -> "String"))
    )(
      """
        |implicit val decoderFieldName: Decoder[FieldName] =
        |  Decoder.forProduct1("value")(FieldName.apply)
        |""".stripMargin,
      """
        |implicit val encoderFieldName: Encoder[FieldName] =
        |  Encoder.forProduct1("value")(_.value)
        |""".stripMargin)

    'monoN - assertGen(
      Class("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]"))
    )(
      """
        |implicit val decoderClass: Decoder[Class] =
        |  Decoder.forProduct2("typeParams", "fields")(Class.apply)
        |""".stripMargin,
      """
        |implicit val encoderClass: Encoder[Class] =
        |  Encoder.forProduct2("typeParams", "fields")(a => (a.typeParams, a.fields))
        |""".stripMargin)

    'poly - assertGen(
      Class("Poly", List("F[_, _[_]]", "A"), List("fa" -> "F[A]"))
    )(
      """
        |implicit def decoderPoly[F[_, _[_]], A]: Decoder[Poly[F, A]] =
        |  Decoder.forProduct1("fa")(Poly.apply)
        |""".stripMargin,
      """
        |implicit def encoderPoly[F[_, _[_]], A]: Encoder[Poly[F, A]] =
        |  Encoder.forProduct1("fa")(_.fa)
        |""".stripMargin)

    'short - assertGen(
      Class("FieldName", Nil, List("value" -> "String")),
      Circe.defaultOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit val decoder: Decoder[FieldName] =
        |  Decoder.forProduct1("value")(FieldName.apply)
        |""".stripMargin,
      """
        |implicit val encoder: Encoder[FieldName] =
        |  Encoder.forProduct1("value")(_.value)
        |""".stripMargin)

    'flat1 - assertGen(
      Class("FieldName", Nil, List("value" -> "String")),
      Circe.defaultOptions.copy(singlesAsObjects = false)
    )(
      """
        |implicit val decoderFieldName: Decoder[FieldName] =
        |  Decoder[String].map(FieldName.apply)
        |""".stripMargin,
      """
        |implicit val encoderFieldName: Encoder[FieldName] =
        |  Encoder[String].contramap(_.value)
        |""".stripMargin)

    'monadic - assertGen(
      Class("X", Nil, List("a" -> "A", "b" -> "B")),
      Circe.defaultOptions.copy(monadicObjects = true)
    )(
      """
        |implicit val decoderX: Decoder[X] =
        |  Decoder.instance { c =>
        |    for {
        |      a <- c.get[A]("a")
        |      b <- c.get[B]("b")
        |    } yield X(a, b)
        |  }
        |""".stripMargin,
      """
        |implicit val encoderX: Encoder[X] =
        |  Encoder.instance { value =>
        |    Json.obj(
        |      "a" -> value.a.asJson,
        |      "b" -> value.b.asJson,
        |    )
        |  }
        |""".stripMargin)
  }
}
