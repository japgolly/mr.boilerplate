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
        Cls("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]")),
        Cls("Field", Nil, List("name" -> "FieldName", "typ" -> "Type")),
        Cls("FieldName", Nil, List("value" -> "String")),
        Cls("Type", Nil, List("v" -> "T")),
      )
    }

    'poly - {
      val input =
      """
        |// blah
        |class Poly[F[_], +A] private (val f: F[A])(implicit x: X) extends Omg[F, A, F]
        |""".stripMargin
      assertParse(input)(
        Cls(
          "Poly",
          List("F[_]", "A"),
          List("f" -> "F[A]")),
      )
    }

    'partial - {
      val input =
      """
        |asdfjhkasgdflkjsa
        |
        |class TagOf[+N <: TopNode] private[vdom](final val tag: String,
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
        Cls(
          "TagOf",
          List("N"),
          List("tag" -> "String", "modifiers" -> "List[Seq[TagMod]]")),
        Cls(
          "HtmlTagOf",
          List("N"),
          List("name" -> "String")),
        Unrecognised("def a"),
      )
    }

    'crash - {
      val input =
        """
          |sealed trait Event
          |
          |/** what? */
          |sealed trait ActiveEvent extends Event
          |
          |case class X(i: Int) extends ActiveEvent
        """.stripMargin
      assertParse(input)(
        Unrecognised("sealed trait Event"),
        Unrecognised("sealed trait ActiveEvent extends Event"),
        Cls("X", Nil, List("i" -> "Int"))
      )
    }

  }
}
