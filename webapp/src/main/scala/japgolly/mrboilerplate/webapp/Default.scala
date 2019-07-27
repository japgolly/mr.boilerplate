package japgolly.mrboilerplate.webapp

import japgolly.mrboilerplate.core.gen._

object Default {

  def input =
    s"""
       |case class Person(name   : PersonName,
       |                  address: Address,
       |                  phone  : Option[PhoneNumber])
       |
       |final case class Roles[F[_], +A <: AnyRef](roles: F[A])
     """.stripMargin.trim

  def options(g: GenDef): g.gen.Options =
    g.foldOptions(

      Circe.Options(
        singlesAsObjects = true,
        monadicObjects = false,
      ),
    )

  def globalOptions = GlobalOptions(
    shortInstanceNames = false,
    generateCompanions = false,
  )

}