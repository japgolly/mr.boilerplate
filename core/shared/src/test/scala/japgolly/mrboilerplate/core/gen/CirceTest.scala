package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core._
import japgolly.mrboilerplate.core.data._
import sourcecode.Line
import utest._

object CirceTest extends TestSuite {
  import CoreTestUtil._
  import UnsafeTypes._

  private val circeOptions = Circe.Options(
    singlesAsObjects = true,
    monadicObjects = false,
    keyConstants = false,
    sumTypes = Circe.Options.SumTypeFormat.TypeToValue,
  )

  private val globalOptions = GlobalOptions(
    shortInstanceNames = false,
    generateCompanions = false,
  )

  private def assertGen(td: TypeDef,
                        opt: Circe.Options = circeOptions,
                        glopt: GlobalOptions = globalOptions,
                       )(expect: String*)
                       (implicit l: Line): Unit = {
    val actual = Circe.gen(opt, glopt)(td)
    assertSeq(actual, expect.map(_.trim))
  }

  override def tests = Tests {

    'mono0 - assertGen(
      Cls("Mono", Nil, Nil, Nil)
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
      Cls("FieldName", Nil, List("value" -> "String"), Nil)
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
      Cls("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]"), Nil)
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
      Cls("NonEmptyList", List("A"), List("head" -> "A", "tail" -> "List[A]"), Nil),
      glopt = globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit def decoder[A: Decoder]: Decoder[NonEmptyList[A]] =
        |  Decoder.forProduct2("head", "tail")(NonEmptyList.apply)
        |""".stripMargin,
      """
        |implicit def encoder[A: Encoder]: Encoder[NonEmptyList[A]] =
        |  Encoder.forProduct2("head", "tail")(a => (a.head, a.tail))
        |""".stripMargin)

    'polyK - assertGen(
      Cls("Poly", List("F[_, _[_]]", "A"), List("fa" -> "F[A]"), Nil)
    )(
      """
        |implicit def decoderPoly[F[_, _[_]], A](implicit ev1: Decoder[F[A]]): Decoder[Poly[F, A]] =
        |  Decoder.forProduct1("fa")(Poly.apply)
        |""".stripMargin,
      """
        |implicit def encoderPoly[F[_, _[_]], A](implicit ev1: Encoder[F[A]]): Encoder[Poly[F, A]] =
        |  Encoder.forProduct1("fa")(_.fa)
        |""".stripMargin)

    'polyK2 - assertGen(
      Cls("PolyK2", List("F[_]", "A", "B"), List("fa" -> "F[A]", "a" -> "A", "b" -> "FFF[B]"), Nil),
      glopt = globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit def decoder[F[_], A: Decoder, B: Decoder](implicit ev1: Decoder[F[A]]): Decoder[PolyK2[F, A, B]] =
        |  Decoder.forProduct3("fa", "a", "b")(PolyK2.apply)
        |""".stripMargin,
      """
        |implicit def encoder[F[_], A: Encoder, B: Encoder](implicit ev1: Encoder[F[A]]): Encoder[PolyK2[F, A, B]] =
        |  Encoder.forProduct3("fa", "a", "b")(a => (a.fa, a.a, a.b))
        |""".stripMargin)

    'short - assertGen(
      Cls("FieldName", Nil, List("value" -> "String"), Nil),
      glopt = globalOptions.copy(shortInstanceNames = true)
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
      Cls("FieldName", Nil, List("value" -> "String"), Nil),
      circeOptions.copy(singlesAsObjects = false)
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
      Cls("X", Nil, List("a" -> "A", "bee" -> "B"), Nil),
      circeOptions.copy(monadicObjects = true)
    )(
      """
        |implicit val decoderX: Decoder[X] =
        |  Decoder.instance { c =>
        |    for {
        |      a   <- c.get[A]("a")
        |      bee <- c.get[B]("bee")
        |    } yield X(a, bee)
        |  }
        |""".stripMargin,
      """
        |implicit val encoderX: Encoder[X] =
        |  Encoder.instance(value => Json.obj(
        |    "a"   -> value.a.asJson,
        |    "bee" -> value.bee.asJson,
        |  ))
        |""".stripMargin)

    'fieldKeysP - assertGen(
      Cls("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]"), Nil),
      circeOptions.copy(keyConstants = true),
      globalOptions.copy(shortInstanceNames = false)
    )(
      """
        |private final val CirceKeyClassTypeParams = "typeParams"
        |private final val CirceKeyClassFields     = "fields"
        |""".stripMargin,
      """
        |implicit val decoderClass: Decoder[Class] =
        |  Decoder.forProduct2(CirceKeyClassTypeParams, CirceKeyClassFields)(Class.apply)
        |""".stripMargin,
      """
        |implicit val encoderClass: Encoder[Class] =
        |  Encoder.forProduct2(CirceKeyClassTypeParams, CirceKeyClassFields)(a => (a.typeParams, a.fields))
        |""".stripMargin)

    'fieldKeysM - assertGen(
      Cls("X", Nil, List("a" -> "A", "bee" -> "B"), Nil),
      circeOptions.copy(monadicObjects = true, keyConstants = true),
      globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |private final val CirceKeyA   = "a"
        |private final val CirceKeyBee = "bee"
        |""".stripMargin,
      """
        |implicit val decoder: Decoder[X] =
        |  Decoder.instance { c =>
        |    for {
        |      a   <- c.get[A](CirceKeyA)
        |      bee <- c.get[B](CirceKeyBee)
        |    } yield X(a, bee)
        |  }
        |""".stripMargin,
      """
        |implicit val encoder: Encoder[X] =
        |  Encoder.instance(value => Json.obj(
        |    CirceKeyA   -> value.a.asJson,
        |    CirceKeyBee -> value.bee.asJson,
        |  ))
        |""".stripMargin)

    'adtSingleKey - assertGen(
      SealedBase("Base", Nil, Nil, List(
        Cls("A", Nil, List("a" -> "Int"), List("Base")),
        Cls("Bee", Nil, List("b" -> "Long"), List("Base")),
      )),
        glopt = globalOptions.copy(shortInstanceNames = true)
      )(
      """
        |implicit val decoder: Decoder[Base] = decodeSumBySoleKey {
        |  case ("a"  , c) => c.as[A]
        |  case ("bee", c) => c.as[Bee]
        |}
      """.stripMargin.trim,
      """
        |implicit val encoder: Encoder[Base] = Encoder.instance {
        |  case a: A   => Json.obj("a"   -> a.asJson)
        |  case a: Bee => Json.obj("bee" -> a.asJson)
        |}
      """.stripMargin.trim,
    )

    'adtUntaggedUnion - assertGen(
      SealedBase("Base", Nil, Nil, List(
        Cls("A", Nil, List("a" -> "Int"), List("Base")),
        Cls("Bee", Nil, List("b" -> "Long"), List("Base")),
      )),
        circeOptions.copy(sumTypes = Circe.Options.SumTypeFormat.UntaggedUnion),
        globalOptions.copy(shortInstanceNames = true)
      )(
      """
        |implicit val decoder: Decoder[Base] =
        |  Decoder[A  ].widen[Base] or
        |  Decoder[Bee].widen[Base]
      """.stripMargin.trim,
      """
        |implicit val encoder: Encoder[Base] = Encoder.instance {
        |  case a: A   => a.asJson
        |  case a: Bee => a.asJson
        |}
      """.stripMargin.trim,
    )

    'adtNoInstances - assertGen(
      SealedBase("Base", Nil, Nil, List(
      SealedBase("A", Nil, List("Base"), Nil),
    )))()

  }
}
