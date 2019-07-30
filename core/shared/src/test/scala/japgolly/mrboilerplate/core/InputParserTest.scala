package japgolly.mrboilerplate.core

import japgolly.mrboilerplate.core.data._
import sourcecode.Line
import utest._

object InputParserTest extends TestSuite {
  import CoreTestUtil._
  import InputParser.Element
  import InputParser.Element.{AbstractClass, Unrecognised}
  import UnsafeTypes._

  private def assertParse(input: String)(expect: Element*)(implicit l: Line): Unit = {
    val p = InputParser.parse(input)
    assertSeq(p, expect)
  }

  override def tests = Tests {

    'empty - assertParse("")()

    'whitespace - assertParse("  \n ")()

    'mono - {
      val input =
      """
        |  package japgolly.mrboilerplate.core
        |
        |// omg
        |
        |/* x */
        |
        |import x._
        |
        |final case class Class(typeParams: List[Type], fields: List[Field])
        |
        |final case class Field(name: FieldName, typ: Type)
        |
        |final case class FieldName( value :
        |String)case   class   Type  (v:T)
        |""".stripMargin
      assertParse(input)(
        Cls("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]"), Nil),
        Cls("Field", Nil, List("name" -> "FieldName", "typ" -> "Type"), Nil),
        Cls("FieldName", Nil, List("value" -> "String"), Nil),
        Cls("Type", Nil, List("v" -> "T"), Nil),
      )
    }

    'poly - {
      val input =
      """
        |// blah
        |class Poly[F[_], +A] private (val f: F[A])(implicit x: X) extends Omg[F, A, F]
        |""".stripMargin
      assertParse(input)(
        Cls("Poly", List("F[_]", "A"), List("f" -> "F[A]"), List("Omg[F, A, F]")),
      )
    }

    'partial - {
      val input =
      """
        |asdfjhkasgdflkjsa
        |
        |class TagOf[ + N <: TopNode] private[vdom](final val tag: String,
        |                                         final val modifiers: List[Seq[TagMod]],
        |) extends VdomElement {
        |
        |  @deprecated("Use .withRef instead", "1.2.0")
        |  def ref[NN >: N <: TopNode](r: Ref.Set[NN]): TagOf[NN] =
        |    (this: TagOf[NN])(Attr.Ref(r))
        |}
        |
        |case class HtmlTagOf[+N <: HtmlTopNode](name: String) extends AnyVal with X{self => }
        |  defa
        |""".stripMargin
      assertParse(input)(
        Unrecognised("asdfjhkasgdflkjsa"),
        Cls("TagOf", List("N"), List("tag" -> "String", "modifiers" -> "List[Seq[TagMod]]"), List("VdomElement")),
        Cls("HtmlTagOf", List("N"), List("name" -> "String"), List("AnyVal", "X")),
        Unrecognised("defa"),
      )
    }

    'unsealed - {
      val input =
        """
          |trait Event
          |
          |/** what? */
          |abstract trait ActiveEvent extends Event
          |
          |// ah
          |abstract class Y1
          |// ah
          |abstract case class Y2()
          |
          |case class X(i: Int)
        """.stripMargin
      assertParse(input)(
        Unrecognised("trait Event"),
        Unrecognised("abstract trait ActiveEvent extends Event"),
        AbstractClass(Cls("Y1", Nil, Nil, Nil)),
        AbstractClass(Cls("Y2", Nil, Nil, Nil)),
        Cls("X", Nil, List("i" -> "Int"), Nil)
      )
    }

    'superTraits - {
      val input =
        """
          |sealed trait Event
          |// ah
          |sealed trait ActiveEvent[A] extends Event
          |sealed protected case class X(i: Int) extends ActiveEvent[List[Int]]
          | with
          |  AnyVal
        """.stripMargin
      val x  = Cls("X", Nil, List("i" -> "Int"), List("ActiveEvent[List[Int]]", "AnyVal"))
      val ae = SealedBase("ActiveEvent", List("A"), List("Event"), List(x))
      val e  = SealedBase("Event", Nil, Nil, List(ae))
      assertParse(input)(e, ae, x)
    }

    'superClasses - {
      val input =
        """
          |sealed abstract class Event
          |// ah
          |sealed abstract class ActiveEvent[A](final val aaa: A) extends Event
          |sealed case class X(i: Int) extends ActiveEvent[List[Int]]
          | with
          |  AnyVal
        """.stripMargin
      val x  = Cls("X", Nil, List("i" -> "Int"), List("ActiveEvent[List[Int]]", "AnyVal"))
      val ae = SealedBase("ActiveEvent", List("A"), List("Event"), List(x))
      val e  = SealedBase("Event", Nil, Nil, List(ae))
      assertParse(input)(e, ae, x)
    }

    'twoDefs - {
      val input =
        """
          |sealed trait A[+W] {
          |  def x(): Iterator[X] = Iterator.empty
          |  def y(): Iterator[Y.Z] = Iterator.empty
          |}
          |// ah
          |sealed trait B[+W] {
          |  def x(): Iterator[X] = Iterator.empty
          |  // ah
          |  def y(): Iterator[Y.Z] = Iterator.empty
          |}
        """.stripMargin
      assertParse(input)(
        SealedBase("A", List("W"), Nil, Nil),
        SealedBase("B", List("W"), Nil, Nil))
    }

    'annotations - {
      val input =
        """
          |@Lenses sealed trait A
          |@Lenses case class B()
        """.stripMargin
      assertParse(input)(
        SealedBase("A", Nil, Nil, Nil),
        Cls("B", Nil, Nil, Nil))
    }

    'decls - {
      val input =
        """
          |type A = X
          |  implicit def univEq[WSR: UnivEq]: UnivEq[Action[WSR]] = UnivEq.derive
          |  implicit val univEqFlat: UnivEq[Flat]                 = UnivEq.derive
          |case class B()
        """.stripMargin
      assertParse(input)(Cls("B", Nil, Nil, Nil))
    }

    'nested - {
      val input =
        """
          |object O {
          |  case class B()
          |  wtf
          |}
          |case class C()
        """.stripMargin
      assertParse(input)(
        Unrecognised("object O {"),
        Cls("B", Nil, Nil, Nil),
        Unrecognised("wtf"),
        Cls("C", Nil, Nil, Nil))
    }

  }
}
