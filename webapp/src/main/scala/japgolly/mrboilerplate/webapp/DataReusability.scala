package japgolly.mrboilerplate.webapp

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.mrboilerplate.core.{Cls, InputParser}
import japgolly.mrboilerplate.core.gen.Generator
import japgolly.scalajs.react.Reusability
import japgolly.univeq.UnivEq
import scala.collection.immutable.ListSet

object DataReusability {

  private def byUnivEq     [A <: AnyRef: UnivEq] = Reusability.by_==[A]
  private def byRefOrUnivEq[A <: AnyRef: UnivEq] = Reusability.byRefOr_==[A]

  implicit def reusabilityListSet[A: Reusability]: Reusability[ListSet[A]] =
    Reusability.byRef || Reusability.byIterator

  implicit def reusabilityNEV[A](implicit v: Reusability[Vector[A]]): Reusability[NonEmptyVector[A]] =
    Reusability.byRef || Reusability.by(_.whole)

  implicit def reusabilityGenerator: Reusability[Generator] =
    Reusability.byRef

  implicit def reusabilityCls: Reusability[Cls] =
    byRefOrUnivEq

  implicit def reusabilityInputParserUnrecognised: Reusability[InputParser.Element.Unrecognised] =
    byRefOrUnivEq

  implicit def reusabilityInputParserElement: Reusability[InputParser.Element] =
    byRefOrUnivEq

}
