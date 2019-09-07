package japgolly.mrboilerplate.core.gen

import japgolly.mrboilerplate.core.data._
import japgolly.mrboilerplate.core.StringUtils._
import monocle.macros.Lenses

object BooPickle extends Generator {

  override val title = "BooPickle"

  @Lenses
  final case class Options(conciseSingleFields: Boolean,
                           objectCodecs: Boolean,
                           keyConstants: Boolean)

  override def initStatements(data: Iterable[TypeDef], opt: Options)(implicit glopt: GlobalOptions): List[String] = {
    var result = "import boopickle.DefaultBasic._" :: Nil
    val text = data.iterator.flatMap(gen(opt)).mkString("\n")
    if (text.contains("ConstPickler"))
      result ::= "import boopickle.ConstPickler"
    result
  }

  override def gen(opt: Options)(implicit glopt: GlobalOptions): TypeDef => List[String] = {
    case c: Cls        => genCls(c, opt)
    case o: Obj        => genObj(o, opt)
    case s: SealedBase => genSB(s, opt)
  }

  // ===================================================================================================================

  private def genObj(obj: Obj, opt: Options)(implicit glopt: GlobalOptions): List[String] = {
    import obj._

    if (opt.objectCodecs)
      s"${mkDef(obj)} =\n  ConstPickler($name)" :: Nil
    else
      Nil
  }

  // ===================================================================================================================

  private def genCls(cls: Cls, opt: Options)(implicit glopt: GlobalOptions): List[String] = {
    import cls._

    val body =
      fieldCount match {
        case 0 =>
          s"ConstPickler($name())"

        case 1 if opt.conciseSingleFields =>
          s"transformPickler($apply)($unapply)"

        case _ =>
          val writes = fields.map(f => s"      state.pickle(a.${f.name})")
          val reads  = fields.map(f => s"      val ${f.name.pad} = state.unpickle[${f.typ}]")
          s"""
             |  new Pickler[$typeNamePoly] {
             |    override def pickle(a: $typeNamePoly)(implicit state: PickleState): Unit = {
             |${writes.mkString("\n")}
             |    }
             |    override def unpickle(implicit state: UnpickleState): $typeNamePoly = {
             |${reads.mkString("\n")}
             |      $name($fieldNames)
             |    }
             |  }
           """.stripMargin.trim
      }

    s"${mkDef(cls)} =\n  $body" :: Nil
  }

  // ===================================================================================================================

  private def genSB(sb: SealedBase, opt: Options)(implicit glopt: GlobalOptions): List[String] = {
    import sb._
    import sb.concreteTransitiveChildren._

    if (children.isEmpty)
      return Nil

    val conciseObjects = !opt.objectCodecs

    val conciseObjectsOnly =
      conciseObjects && children.forall(_.isInstanceOf[Obj])

    def cases(f: (TypeDef.Concrete, Int) => String) =
      children.iterator.zipWithIndex.map(f.tupled).mkString("\n")

    def mkKey(t: TypeDef.Concrete) =
      "Key" + maxTailSuffixLen.pad(t.tailSuffix)

    val keys =
      if (opt.keyConstants)
        "\n" + cases((t, k) => s"    private[this] final val ${mkKey(t)} = $k")
      else
        ""

    def write(t: TypeDef.Concrete, key: Int) = {
      val maxLen = maxCaseTypeLen
      var suffix = "; state.pickle(b)"
      val caseClause = t match {
        case c: Cls => s"b: ${maxLen.pad(c.typeNamePoly)}"
        case o: Obj =>
          if (conciseObjects) suffix = ""
          if (conciseObjectsOnly)  maxLen.pad(o.name)
          else if (conciseObjects) s"${maxLen.pad(o.name)}   "
          else                     s"b@ ${maxLen.pad(o.name)}"
      }
      val k = if (opt.keyConstants) mkKey(t) else key
      s"        case $caseClause => state.enc.writeByte($k)$suffix"
    }

    def read(t: TypeDef.Concrete, key: Int) = {
      val k = if (opt.keyConstants) mkKey(t) else key
      val body = t match {
        case o: Obj if conciseObjects => o.name
        case _                        => s"state.unpickle[${t.typeNamePoly}]"
      }
      s"        case $k => $body"
    }

    s"""${mkDef(sb)} =
       |  new Pickler[$typeNamePoly] {$keys
       |    override def pickle(a: $typeNamePoly)(implicit state: PickleState): Unit =
       |      a match {
       |${cases(write)}
       |      }
       |    override def unpickle(implicit state: UnpickleState): $typeNamePoly =
       |      state.dec.readByte match {
       |${cases(read)}
       |      }
       |  }
     """.stripMargin.trim :: Nil
  }

  // ===================================================================================================================

  private def mkDef(td: TypeDef)(implicit g: GlobalOptions): String = {
    import td._
    s"implicit $valDef pickler$instanceNameSuffix${typeParamDefsAndEvTC("Pickler")}: Pickler[$typeNamePoly]"
  }
}