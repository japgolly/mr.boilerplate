package japgolly.mrboilerplate.core

import japgolly.mrboilerplate.base.test.BaseTestUtil._
import sourcecode.Line
import utest._

object InputParserTest extends TestSuite {
  import UnsafeTypes._

  private def assertParse(input: String)(classes: Class*)(implicit l: Line): Unit = {
    val p = InputParser.parseText(input)
    assertSeq(p, classes)
  }

  override def tests = Tests {

    'mono - {
      val input =
      """
        |package japgolly.mrboilerplate.core
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

  }
}
