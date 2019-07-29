package japgolly.mrboilerplate.core

import japgolly.mrboilerplate.core.data._
import sourcecode.Line
import utest._

object InputParserTest extends TestSuite {
  import CoreTestUtil._
  import InputParser.Element
  import InputParser.Element.Unrecognised
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
        |  def a
        |""".stripMargin
      assertParse(input)(
        Unrecognised("asdfjhkasgdflkjsa"),
        Cls("TagOf", List("N"), List("tag" -> "String", "modifiers" -> "List[Seq[TagMod]]"), List("VdomElement")),
        Cls("HtmlTagOf", List("N"), List("name" -> "String"), List("AnyVal", "X")),
        Unrecognised("def a"),
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
          |case class X(i: Int)
        """.stripMargin
      assertParse(input)(
        Unrecognised("trait Event"),
        Unrecognised("abstract trait ActiveEvent extends Event"),
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
      assertParse(input)(
        SealedBase("Event", Nil, Nil),
        SealedBase("ActiveEvent", List("A"), List("Event")),
        Cls("X", Nil, List("i" -> "Int"), List("ActiveEvent[List[Int]]", "AnyVal"))
      )
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
      assertParse(input)(
        SealedBase("Event", Nil, Nil),
        SealedBase("ActiveEvent", List("A"), List("Event")),
        Cls("X", Nil, List("i" -> "Int"), List("ActiveEvent[List[Int]]", "AnyVal"))
      )
    }

  }
}
