package japgolly.mrboilerplate.webapp

import japgolly.mrboilerplate.core.gen._

object Default {

  def input =
    s"""
       |sealed trait Sum
       |
       |case class Empty() extends Sum
       |
       |case class Person(name   : String,
       |                  address: String,
       |                  phone  : Option[String]) extends Sum
       |
       |final case class NonEmptyList[+A](head: A, tail: List[A])
       |
       |final case class Roles[F[_], A](roles: F[A])
     """.stripMargin.trim + "\n"

  def options(g: GeneratorDef): g.gen.Options =
    g.foldOptions((

      Circe.Options(
        singlesAsObjects = true,
        monadicObjects = false,
        keyConstants = false,
        sumTypes = Circe.Options.SumTypeFormat.UntaggedUnion,
      ),

      UnivEqGen.Options(
        oneLine = false,
      ),

    ))

  def globalOptions = GlobalOptions(
    shortInstanceNames = false,
    generateCompanions = false,
    makeValsLazy = true,
  )

}
