package japgolly.mrboilerplate.core.data

import japgolly.univeq.UnivEq

final case class SealedBase(name      : String,
                            typeParams: List[Type],
                            superTypes: List[Type]) {

  override def toString: String = {
    val tp = if (typeParams.isEmpty) "" else typeParams.mkString("[", ", ", "]")
    val ex = if (superTypes.isEmpty) "" else superTypes.mkString(" extends ", " with ", "")
    s"sealed _____ $name$tp$ex"
  }


}

object SealedBase {
  implicit def univEq: UnivEq[SealedBase] = UnivEq.derive
}