package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core._
import japgolly.mrboilerplate.core.data._
import sourcecode.Line
import utest._

object BooPickleTest extends TestSuite {
  import CoreTestUtil._
  import UnsafeTypes._

  private val booPickleOptions = BooPickle.Options(
    conciseSingleFields = true,
    keyConstants = false,
  )

  private val globalOptions = GlobalOptions(
    shortInstanceNames = false,
    generateCompanions = false,
    makeValsLazy = false,
  )

  private def assertGen(td: TypeDef,
                        opt: BooPickle.Options = booPickleOptions,
                        glopt: GlobalOptions = globalOptions,
                       )(expect: String*)
                       (implicit l: Line): Unit = {
    val actual = BooPickle.gen(opt)(glopt)(td)
    val expect2 = expect.map(_.trim)
    if (actual.size == 1 && expect2.size == 1)
      assertMultiline(actual.head, expect2.head)
    else
      assertSeq(actual, expect2)
  }

  override def tests = Tests {

    'obj - assertGen(
      Obj("X", Nil)
    )(
      """
        |implicit val picklerX: Pickler[X.type] =
        |  ConstPickler(X)
        |""".stripMargin)

    'mono0 - assertGen(
      Cls("Mono", Nil, Nil, Nil)
    )(
      """
        |implicit val picklerMono: Pickler[Mono] =
        |  ConstPickler(Mono())
        |""".stripMargin)

    'mono1 - assertGen(
      Cls("Username", Nil, List("value" -> "String"), Nil)
    )(
      """
        |implicit val picklerUsername: Pickler[Username] =
        |  transformPickler(Username.apply)(_.value)
        |""".stripMargin)

    'monoN - assertGen(
      Cls("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]"), Nil)
    )(
      """
        |implicit val picklerClass: Pickler[Class] =
        |  new Pickler[Class] {
        |    override def pickle(a: Class)(implicit state: PickleState): Unit = {
        |      state.pickle(a.typeParams)
        |      state.pickle(a.fields)
        |    }
        |    override def unpickle(implicit state: UnpickleState): Class = {
        |      val typeParams = state.unpickle[List[Type]]
        |      val fields     = state.unpickle[List[Field]]
        |      Class(typeParams, fields)
        |    }
        |  }
        |""".stripMargin)

    'poly - assertGen(
      Cls("NonEmptyList", List("A"), List("head" -> "A", "tail" -> "List[A]"), Nil),
      glopt = globalOptions.copy(shortInstanceNames = true)
    )(
      """
        |implicit def pickler[A: Pickler]: Pickler[NonEmptyList[A]] =
        |  new Pickler[NonEmptyList[A]] {
        |    override def pickle(a: NonEmptyList[A])(implicit state: PickleState): Unit = {
        |      state.pickle(a.head)
        |      state.pickle(a.tail)
        |    }
        |    override def unpickle(implicit state: UnpickleState): NonEmptyList[A] = {
        |      val head = state.unpickle[A]
        |      val tail = state.unpickle[List[A]]
        |      NonEmptyList(head, tail)
        |    }
        |  }
        |""".stripMargin)

    'adt - assertGen(
      SealedBase("Base", Nil, Nil, List(
        Cls("A", Nil, List("a" -> "Int"), List("Base")),
        Cls("Bee", Nil, List("b" -> "Long"), List("Base")),
        Obj("O", Nil),
      )))(
      """
        |implicit val picklerBase: Pickler[Base] =
        |  new Pickler[Base] {
        |    override def pickle(a: Base)(implicit state: PickleState): Unit =
        |      a match {
        |        case b: A   => state.enc.writeByte(0); state.pickle(b)
        |        case b: Bee => state.enc.writeByte(1); state.pickle(b)
        |        case b@ O   => state.enc.writeByte(2); state.pickle(b)
        |      }
        |    override def unpickle(implicit state: UnpickleState): Base =
        |      state.dec.readByte match {
        |        case 0 => state.unpickle[A]
        |        case 1 => state.unpickle[Bee]
        |        case 2 => state.unpickle[O.type]
        |      }
        |  }
      """.stripMargin.trim)

    'adtWithKeys - assertGen(
      SealedBase("Base", Nil, Nil, List(
        Cls("A", Nil, List("a" -> "Int"), List("Base")),
        Cls("Bee", Nil, List("b" -> "Long"), List("Base")),
        Obj("O", Nil),
      )),
      booPickleOptions.copy(keyConstants = true)
    )(
      """
        |implicit val picklerBase: Pickler[Base] =
        |  new Pickler[Base] {
        |    private[this] final val KeyA   = 0
        |    private[this] final val KeyBee = 1
        |    private[this] final val KeyO   = 2
        |    override def pickle(a: Base)(implicit state: PickleState): Unit =
        |      a match {
        |        case b: A   => state.enc.writeByte(KeyA  ); state.pickle(b)
        |        case b: Bee => state.enc.writeByte(KeyBee); state.pickle(b)
        |        case b@ O   => state.enc.writeByte(KeyO  ); state.pickle(b)
        |      }
        |    override def unpickle(implicit state: UnpickleState): Base =
        |      state.dec.readByte match {
        |        case KeyA   => state.unpickle[A]
        |        case KeyBee => state.unpickle[Bee]
        |        case KeyO   => state.unpickle[O.type]
        |      }
        |  }
      """.stripMargin.trim)

    'adtNoInstances - assertGen(
      SealedBase("Base", Nil, Nil, List(
      SealedBase("A", Nil, List("Base"), Nil),
    )))()

  }
}
