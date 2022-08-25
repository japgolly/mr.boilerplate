package japgolly.mrboilerplate.core

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.mrboilerplate.core.data._

private object PostProcess {

  /** Populates the [[.directChildren]] field of [[SealedBase]] instances.
    */
  def apply[A](as: List[A])
              (toTypeDef: A => Option[TypeDef],
               fromTypeDef: TypeDef => A): List[A] = {

    val tmp = as.iterator.map(toTypeDef).toArray
    val indices = tmp.indices

    val direct: SealedBase => List[TypeDef] =
      src => {
        val srcType = Type(src.name)
        tmp.iterator.filter(_.forall(_ ne src)).collect {
          case Some(c) if c.superTypes.exists(_.isInstanceOfType(srcType)) => c
        }.toList
      }

    // Doesn't handle cycles and I don't care right now

    MutableArray(indices)
      .sortBySchwartzian(tmp(_).fold(0)(-_.superTypes.size))
      .iterator()
      .foreach { i =>
        tmp(i) match {
          case Some(s: SealedBase) => tmp(i) = Some(s.copy(directChildren = direct(s)))
          case _ => ()
        }
      }

    indices.iterator.map(i => tmp(i).fold(as(i))(fromTypeDef)).toList
  }

}
