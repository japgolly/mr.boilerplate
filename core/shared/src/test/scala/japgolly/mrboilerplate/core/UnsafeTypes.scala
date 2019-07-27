package japgolly.mrboilerplate.core

import scala.collection.generic.CanBuildFrom

object UnsafeTypes {

  implicit def fieldNameFromStr(s: String): FieldName =
    FieldName(s)

  implicit def typeFromStr(s: String): Type =
    Type(s)

  implicit def fieldFromStrStr(s: (String, String)): Field =
    Field(s._1, s._2)

  implicit def fieldsFromStrStr[F[x] <: Traversable[x], A](s: F[A])(implicit f: A => Field, cbf: CanBuildFrom[Nothing, Field, F[Field]]): F[Field] =
    (cbf.apply() ++= s.toIterator.map(f)).result()

  implicit def inputParserElementL(a: InputParser.Unrecognised): InputParser.Element =
    Left(a)

  implicit def inputParserElementR(a: Cls): InputParser.Element =
    Right(a)
}
