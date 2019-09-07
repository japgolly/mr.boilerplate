package japgolly.mrboilerplate.core

import scala.collection.compat._
import japgolly.mrboilerplate.core.data._

object UnsafeTypes {

  implicit def fieldNameFromStr(s: String): FieldName =
    FieldName(s)

  implicit def typeFromStr(s: String): Type =
    Type(s)

  implicit def fieldFromStrStr(s: (String, String)): Field =
    Field(s._1, s._2)

  implicit def fieldsFromStrStr[F[x] <: Iterable[x], A](s: F[A])(implicit f: A => Field, cbf: Factory[Field, F[Field]]): F[Field] =
    (cbf.newBuilder ++= s.iterator.map(f)).result()

  implicit def inputParserElementTD(a: TypeDef): InputParser.Element =
    InputParser.Element.Success(a)
}
