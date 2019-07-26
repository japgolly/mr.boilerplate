package japgolly.mrboilerplate.core

import sourcecode.Line
import utest._

object InputParserTest extends TestSuite {
  import CoreTestUtil._
  import InputParser.{Element, Unrecognised}
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
        Class("Class", Nil, List("typeParams" -> "List[Type]", "fields" -> "List[Field]")),
        Class("Field", Nil, List("name" -> "FieldName", "typ" -> "Type")),
        Class("FieldName", Nil, List("value" -> "String")),
        Class("Type", Nil, List("v" -> "T")),
      )
    }

    'poly - {
      val input =
      """
        |// blah
        |class Poly[F[_], +A] private (val f: F[A])(implicit x: X) extends Omg[F, A, F]
        |""".stripMargin
      assertParse(input)(
        Class(
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
        Class(
          "TagOf",
          List("N"),
          List("tag" -> "String", "modifiers" -> "List[Seq[TagMod]]")),
        Class(
          "HtmlTagOf",
          List("N"),
          List("name" -> "String")),
        Unrecognised("def a"),
      )
    }

  }
}
