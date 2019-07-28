package japgolly.mrboilerplate.webapp

import japgolly.mrboilerplate.core.gen._

object Default {

  def input =
    s"""
       |case class Person(name   : PersonName,
       |                  address: Address,
       |                  phone  : Option[PhoneNumber])
       |
       |final case class NonEmptyList[A](head: A, tail: List[A])
       |
       |final case class Roles[F[_], +A <: AnyRef](roles: F[A])
     """.stripMargin.trim + "\n"

  def options(g: GeneratorDef): g.gen.Options =
    g.foldOptions((

      Circe.Options(
        singlesAsObjects = true,
        monadicObjects = false,
        keyConstants = false,
      ),

      UnivEqGen.Options(
        oneLine = false,
      ),

    ))

  def globalOptions = GlobalOptions(
    shortInstanceNames = true,
    generateCompanions = true,
  )

}
