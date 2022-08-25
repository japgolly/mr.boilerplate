package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core._
import japgolly.mrboilerplate.core.data._
import sourcecode.Line
import utest._

object JsonCodecTest extends TestSuite {
  import CoreTestUtil._
  import UnsafeTypes._

  private val JsonCodecOptions = JsonCodec.Options(
    objectCodecs = true,
    singlesAsObjects = true,
    monadicObjects = false,
    keyConstants = false,
    sumTypes = JsonCodec.Options.SumTypeFormat.TypeToValue,
  )

  private val globalOptions = GlobalOptions(
    shortInstanceNames = false,
    generateCompanions = false,
    makeValsLazy = false,
  )

  private def assertGen(td: TypeDef,
                        opt: JsonCodec.Options = JsonCodecOptions,
                        glopt: GlobalOptions = globalOptions,
                       )(expect: String*)
                       (implicit l: Line): Unit = {
    val actual = JsonCodec.gen(opt)(glopt)(td)
    val expect2 = expect.map(_.trim)
    if (actual.size == expect2.size)
      actual.indices.foreach(i => assertMultiline(actual(i), expect2(i)))
    else
      assertSeq(actual, expect2)
  }

  override def tests = Tests {

    "obj" - assertGen(
      Obj("Obj", Nil)
    )(
      """
        |implicit val jsonCodecObj: JsonCodec[Obj.type] =
        |  JsonCodec.const(Obj)
        |""".stripMargin)

    "objOff" - assertGen(
      Obj("Obj", Nil),
      JsonCodecOptions.copy(objectCodecs = false)
    )()

    "mono0" - assertGen(
      Cls("Mono", Nil, Nil, Nil)
    )(
      """
        |implicit val jsonCodecMono: JsonCodec[Mono] =
        |  JsonCodec.const(Mono())
        |""".stripMargin)

    "mono1" - assertGen(
      Cls("FieldName", Nil, List("value" -> "String"), Nil)
    )(
      """
        |implicit val jsonCodecFieldName: JsonCodec[FieldName] = {
        |  val enc = Encoder.forProduct1("value")(_.value)
        |  val dec = Decoder.forProduct1("value")(FieldName.apply)
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "monoN" - assertGen(
      Cls("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]"), Nil)
    )(
      """
        |implicit val jsonCodecClass: JsonCodec[Class] = {
        |  val enc = Encoder.forProduct2("typeParams", "fields")(a => (a.typeParams, a.fields))
        |  val dec = Decoder.forProduct2("typeParams", "fields")(Class.apply)
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "poly" - assertGen(
      Cls("NonEmptyList", List("A"), List("head" -> "A", "tail" -> "List[A]"), Nil),
      glopt = globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit def jsonCodec[A: Decoder: Encoder]: JsonCodec[NonEmptyList[A]] = {
        |  val enc = Encoder.forProduct2("head", "tail")(a => (a.head, a.tail))
        |  val dec = Decoder.forProduct2("head", "tail")(NonEmptyList.apply[A])
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "polyK" - assertGen(
      Cls("Poly", List("F[_, _[_]]", "A"), List("fa" -> "F[A]"), Nil)
    )(
      """
        |implicit def jsonCodecPoly[F[_, _[_]], A](implicit d1: Decoder[F[A]], e1: Encoder[F[A]]): JsonCodec[Poly[F, A]] = {
        |  val enc = Encoder.forProduct1("fa")(_.fa)
        |  val dec = Decoder.forProduct1("fa")(Poly.apply[F, A])
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "polyK2" - assertGen(
      Cls("PolyK2", List("F[_]", "A", "B"), List("fa" -> "F[A]", "a" -> "A", "b" -> "FFF[B]"), Nil),
      glopt = globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit def jsonCodec[F[_], A: Decoder: Encoder, B: Decoder: Encoder](implicit d1: Decoder[F[A]], e1: Encoder[F[A]]): JsonCodec[PolyK2[F, A, B]] = {
        |  val enc = Encoder.forProduct3("fa", "a", "b")(a => (a.fa, a.a, a.b))
        |  val dec = Decoder.forProduct3("fa", "a", "b")(PolyK2.apply[F, A, B])
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "short" - assertGen(
      Cls("FieldName", Nil, List("value" -> "String"), Nil),
      glopt = globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit val jsonCodec: JsonCodec[FieldName] = {
        |  val enc = Encoder.forProduct1("value")(_.value)
        |  val dec = Decoder.forProduct1("value")(FieldName.apply)
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "flat1" - assertGen(
      Cls("FieldName", Nil, List("value" -> "String"), Nil),
      JsonCodecOptions.copy(singlesAsObjects = false)
    )(
      """
        |implicit val jsonCodecFieldName: JsonCodec[FieldName] =
        |  JsonCodec.summon[String].xmap(FieldName.apply)(_.value)
        |""".stripMargin)

    "monadic" - assertGen(
      Cls("X", Nil, List("a" -> "A", "bee" -> "B"), Nil),
      JsonCodecOptions.copy(monadicObjects = true)
    )(
      """implicit val jsonCodecX: JsonCodec[X] = {
        |  val enc = Encoder.instance(value => Json.obj(
        |    "a"   -> value.a.asJson,
        |    "bee" -> value.bee.asJson,
        |  ))
        |  val dec = Decoder.instance { c =>
        |    for {
        |      a   <- c.get[A]("a")
        |      bee <- c.get[B]("bee")
        |    } yield X(a, bee)
        |  }
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "fieldKeysP" - assertGen(
      Cls("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]"), Nil),
      JsonCodecOptions.copy(keyConstants = true),
      globalOptions.copy(shortInstanceNames = false)
    )(
      """
        |private final val JsonCodecKeyClassTypeParams = "typeParams"
        |private final val JsonCodecKeyClassFields     = "fields"
        |""".stripMargin,
      """
        |implicit val jsonCodecClass: JsonCodec[Class] = {
        |  val enc = Encoder.forProduct2(JsonCodecKeyClassTypeParams, JsonCodecKeyClassFields)(a => (a.typeParams, a.fields))
        |  val dec = Decoder.forProduct2(JsonCodecKeyClassTypeParams, JsonCodecKeyClassFields)(Class.apply)
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "fieldKeysM" - assertGen(
      Cls("X", Nil, List("a" -> "A", "bee" -> "B"), Nil),
      JsonCodecOptions.copy(monadicObjects = true, keyConstants = true),
      globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |private final val JsonCodecKeyA   = "a"
        |private final val JsonCodecKeyBee = "bee"
        |""".stripMargin,
      """
        |implicit val jsonCodec: JsonCodec[X] = {
        |  val enc = Encoder.instance(value => Json.obj(
        |    JsonCodecKeyA   -> value.a.asJson,
        |    JsonCodecKeyBee -> value.bee.asJson,
        |  ))
        |  val dec = Decoder.instance { c =>
        |    for {
        |      a   <- c.get[A](JsonCodecKeyA)
        |      bee <- c.get[B](JsonCodecKeyBee)
        |    } yield X(a, bee)
        |  }
        |  JsonCodec(enc, dec)
        |}
        |""".stripMargin)

    "adtSingleKey" - assertGen(
      SealedBase("Base", Nil, Nil, List(
        Cls("A", Nil, List("a" -> "Int"), List("Base")),
        Cls("Bee", Nil, List("b" -> "Long"), List("Base")),
        Obj("O", Nil),
      )),
        glopt = globalOptions.copy(shortInstanceNames = true)
      )(
      """
        |implicit val jsonCodec: JsonCodec[Base] = {
        |  val enc = Encoder.instance {
        |    case a: A   => Json.obj("a"   -> a.asJson)
        |    case a: Bee => Json.obj("bee" -> a.asJson)
        |    case a@ O   => Json.obj("o"   -> a.asJson)
        |  }
        |  val dec = decodeSumBySoleKey {
        |    case ("a"  , c) => c.as[A]
        |    case ("bee", c) => c.as[Bee]
        |    case ("o"  , c) => c.as[O.type]
        |  }
        |  JsonCodec(enc, dec)
        |}
      """.stripMargin.trim,
    )

    "adtSingleKeyNoObj" - assertGen(
      SealedBase("Base", Nil, Nil, List(
        Cls("A", Nil, List("a" -> "Int"), List("Base")),
        Cls("Bee", Nil, List("b" -> "Long"), List("Base")),
        Obj("O", Nil),
      )),
        JsonCodecOptions.copy(objectCodecs = false),
        glopt = globalOptions.copy(shortInstanceNames = true)
      )(
      """implicit val jsonCodec: JsonCodec[Base] = {
        |  val enc = Encoder.instance {
        |    case a: A   => Json.obj("a"   -> a.asJson)
        |    case a: Bee => Json.obj("bee" -> a.asJson)
        |    case O      => Json.obj("o"   -> ().asJson)
        |  }
        |  val dec = decodeSumBySoleKey {
        |    case ("a"  , c) => c.as[A]
        |    case ("bee", c) => c.as[Bee]
        |    case ("o"  , _) => Right(O)
        |  }
        |  JsonCodec(enc, dec)
        |}
      """.stripMargin.trim,
    )

    "adtObjOnlyNoCodec" - assertGen(
      SealedBase("Base", Nil, Nil, List(
        Obj("Bee", Nil),
        Obj("O", Nil),
      )),
        JsonCodecOptions.copy(objectCodecs = false),
        glopt = globalOptions.copy(shortInstanceNames = true)
      )(
      """
        |implicit val jsonCodec: JsonCodec[Base] = {
        |  val enc = Encoder.instance {
        |    case Bee => Json.obj("bee" -> ().asJson)
        |    case O   => Json.obj("o"   -> ().asJson)
        |  }
        |  val dec = decodeSumBySoleKey {
        |    case ("bee", _) => Right(Bee)
        |    case ("o"  , _) => Right(O)
        |  }
        |  JsonCodec(enc, dec)
        |}
      """.stripMargin.trim,
    )

    "adtUntaggedUnion" - assertGen(
      SealedBase("Base", Nil, Nil, List(
        Cls("A", Nil, List("a" -> "Int"), List("Base")),
        Cls("Bee", Nil, List("b" -> "Long"), List("Base")),
        Obj("O", Nil),
      )),
        JsonCodecOptions.copy(sumTypes = JsonCodec.Options.SumTypeFormat.UntaggedUnion),
        globalOptions.copy(shortInstanceNames = true)
      )(
      """
        |implicit val jsonCodec: JsonCodec[Base] = {
        |  val enc = Encoder.instance {
        |    case a: A   => a.asJson
        |    case a: Bee => a.asJson
        |    case a@ O   => a.asJson
        |  }
        |  val dec =
        |    Decoder[A     ].widen[Base] or
        |    Decoder[Bee   ].widen[Base] or
        |    Decoder[O.type].widen[Base]
        |  JsonCodec(enc, dec)
        |}
      """.stripMargin.trim,
    )

    "adtNoInstances" - assertGen(
      SealedBase("Base", Nil, Nil, List(
      SealedBase("A", Nil, List("Base"), Nil),
    )))()

    "adtPoly" - assertGen(
      SealedBase("Base", List("X", "Y"), Nil, List(
        Cls("A", List("X"), List("a" -> "Int"), List("Base")),
        Cls("Bee", List("Y, X"), List("b" -> "Long"), List("Base")),
        Obj("O", Nil),
      )),
      glopt = globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit def jsonCodec[X, Y](implicit d1: Decoder[A[X]], d2: Decoder[Bee[Y, X]], e1: Encoder[A[X]], e2: Encoder[Bee[Y, X]]): JsonCodec[Base[X, Y]] = {
        |  val enc = Encoder.instance {
        |    case a: A[X]      => Json.obj("a"   -> a.asJson)
        |    case a: Bee[Y, X] => Json.obj("bee" -> a.asJson)
        |    case a@ O         => Json.obj("o"   -> a.asJson)
        |  }
        |  val dec = decodeSumBySoleKey {
        |    case ("a"  , c) => c.as[A[X]]
        |    case ("bee", c) => c.as[Bee[Y, X]]
        |    case ("o"  , c) => c.as[O.type]
        |  }
        |  JsonCodec(enc, dec)
        |}
      """.stripMargin.trim,
    )

    "adtWithPrefix" - assertGen(
      SealedBase("Qqq.Base", List("X", "Y"), Nil, List(
        Cls("Qqq.Bee", List("Y, X"), List("b" -> "Long"), List("Base")),
        Obj("Qqq.O", Nil),
      )),
      glopt = globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit def jsonCodec[X, Y](implicit d1: Decoder[Qqq.Bee[Y, X]], e1: Encoder[Qqq.Bee[Y, X]]): JsonCodec[Qqq.Base[X, Y]] = {
        |  val enc = Encoder.instance {
        |    case a: Qqq.Bee[Y, X] => Json.obj("bee" -> a.asJson)
        |    case a@ Qqq.O         => Json.obj("o"   -> a.asJson)
        |  }
        |  val dec = decodeSumBySoleKey {
        |    case ("bee", c) => c.as[Qqq.Bee[Y, X]]
        |    case ("o"  , c) => c.as[Qqq.O.type]
        |  }
        |  JsonCodec(enc, dec)
        |}
      """.stripMargin.trim,
    )

  }
}
