package japgolly.mrboilerplate.core

import fastparse._
import fastparse.MultiLineWhitespace._
import japgolly.mrboilerplate.core.data._
import japgolly.univeq.UnivEq

object InputParser {

  private final case class Flags(isSealed: Boolean, isAbstract: Boolean)
  private object Flags {
    val empty = Flags(false, false)

    // lol
    def fromStr(s: String): Flags =
      Flags(
        isSealed = s.contains("sealed"),
        isAbstract = s.contains("abstract"),
      )
  }

  private object Scala
    extends scalaparse.Core
      with scalaparse.Types
      with scalaparse.Exprs {

    def TypeArg2[_: P]: P[Type] = {
      def CtxBounds = P((`<%` ~/ Type).rep ~ (`:` ~/ Type).rep)
      P(((Id | `_`) ~ TypeArgList.?).!.map(s => data.Type(s.trim)) ~ TypeBounds ~ CtxBounds)
    }

    def TypeArgList2[_: P]: P[Seq[Type]] = {
      def Variant: P[Type] = P( Annot.rep ~ CharIn("+\\-").? ~ TypeArg2 )
      P( "[" ~/ Variant.repTC(1) ~ "]" )
    }

    def TmplBody[_: P]: P[Unit] = {
      def Prelude = P( (Annot ~ OneNLMax).rep ~ Mod./.rep )
      def TmplStat = P( Import | Prelude ~ BlockDef | StatCtx.Expr )

      P( "{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = NoCut(Semis)) ~ Semis.? ~ `}` )
    }

    def ValVarDef[_: P] = P( BindPattern.rep(1, ","./) ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr).? )

    def FunDef[_: P] = {
      def Body = P( WL ~ `=` ~/ `macro`.? ~ StatCtx.Expr | OneNLMax ~ "{" ~ Block ~ "}" )
      P( FunSig ~ (`:` ~/ Type).? ~~ Body.? )
    }

    def BlockDef[_: P]: P[Unit] = P( Dcl | TraitDef  | ClsDef | ObjDef )

    def ClsDef[_: P]: P[(String, Option[Seq[Type]], Seq[Seq[(String, String)]], Seq[Type])] = {
      def ClsAnnot = P( `@` ~ SimpleType ~ ArgList.? )
      def Prelude = P( NotNewline ~ ( ClsAnnot.rep(1) ~ AccessMod.? | AccessMod) )
      def ClsArgMod = P( Mod.rep ~ (`val` | `var`) )
      def ClsArg = P( Annot.rep ~ ClsArgMod.? ~ Id.! ~ `:` ~ Type.! ~ (`=` ~ ExprCtx.Expr).? )
      def ClsArgs = P( OneNLMax ~ "(" ~/ `implicit`.? ~ ClsArg.repTC() ~ ")" )

      P( `case`.? ~ `class` ~/ Id.! ~ TypeArgList2.? ~~ Prelude.? ~~ ClsArgs.repX ~ DefTmpl.?.map(_.getOrElse(Seq.empty)) )
    }

    def Constrs[_: P]: P[Seq[data.Type]] =
      P( (WL ~ Constr).rep(1, `with`./) )

    def EarlyDefTmpl[_: P]: P[Seq[data.Type]] =
      P( TmplBody ~ (`with` ~/ Constr).rep ~ TmplBody.? )

    def NamedTmpl[_: P]: P[Seq[data.Type]] =
      P( Constrs ~ TmplBody.? )

    def DefTmpl[_: P]: P[Seq[data.Type]] =
      P( (`extends` | `<:`) ~ AnonTmpl2 | TmplBody.map(_ => Seq.empty) )

    def AnonTmpl2[_: P]: P[Seq[data.Type]] =
      P( EarlyDefTmpl | NamedTmpl | TmplBody.map(_ => Seq.empty) )

    override def AnonTmpl[_: P] = AnonTmpl2.map(_ => ())

    def TraitDef[_: P]: P[(String, Option[Seq[Type]], Seq[Type])] =
      P( `trait` ~/ Id.! ~ TypeArgList2.? ~ DefTmpl.?.map(_.getOrElse(Seq.empty)) )

    def ObjDef[_: P]: P[Unit] = P( `case`.? ~ `object` ~/ Id ~ DefTmpl.? )

    def Constr[_: P] = P( AnnotType2 ~~ (NotNewline ~ ParenArgList ).repX )
    def AnnotType2[_: P] = P(SimpleType.!.map(s => data.Type(s.trim)) ~~ NLAnnot.repX )

//    def PkgObj[_: P] = P( ObjDef )
//    def PkgBlock[_: P] = P( QualId ~/ `{` ~ TopStatSeq.? ~ `}` )
//    def Pkg[_: P] = P( `package` ~/ (PkgBlock | PkgObj) )
//    def TopStatSeq[_: P]: P[Unit] = {
//      def Tmpl = P( (Annot ~~ OneNLMax).rep ~ Mod.rep ~ (TraitDef | ClsDef | ObjDef) )
//      def TopStat = P( Pkg | Import | Tmpl )
//      P( TopStat.repX(1, Semis) )
//    }
    def TopPkgSeq[_: P] = P( ((`package` ~ QualId) ~~ !(WS ~ "{")).repX(1, Semis) )
//    def CompilationUnit[_: P]: P[Unit] = {
//      def Body = P( TopPkgSeq ~~ (Semis ~ TopStatSeq).? | TopStatSeq )
//      P( Semis.? ~ Body.? ~~ Semis.? ~ WL0 ~ End )
//    }
  }

  // ===================================================================================================================

  private def flags[_: P]: P[Flags] =
    P(Scala.Mod.rep.!.map(Flags.fromStr))

  private def cls[_: P]: P[Element] =
    P(flags ~ Scala.ClsDef).map {
      case (f, (name, types, fields, sup)) =>
        if (f.isSealed && f.isAbstract)
          Element.Success(SealedBase(
            name           = name,
            typeParams     = types.iterator.flatten.toList,
            superTypes     = sup.toList,
            directChildren = Nil,
          ))
        else {
          val cls = Cls(
            name       = name,
            typeParams = types.iterator.flatten.toList,
            fields     = fields.iterator.take(1).flatten.map { case (n, t) => Field(FieldName(n), Type(t)) }.toList,
            superTypes = sup.toList,
          )
          if (f.isAbstract)
            Element.AbstractClass(cls)
          else
            Element.Success(cls)
        }
    }

  private def sealedTrait[_: P]: P[Element] =
    P(flags.filter(_.isSealed) ~ Scala.TraitDef).map {
      case (_, (name, types, sup)) =>
        Element.Success(SealedBase(
          name           = name,
          typeParams     = types.iterator.flatten.toList,
          superTypes     = sup.toList,
          directChildren = Nil,
        ))
    }

  private def ignore[_: P]: P[Unit] =
    P(Scala.TopPkgSeq | Scala.Import | Scala.Literals.Comment)

  private def recognised[_: P]: P[Element] =
    P(cls | sealedTrait | ignore.rep(1).map(_ => Element.Empty))

  private def unrecognised[_: P]: P[Element] =
    P((!recognised ~~ (CharPred(_.isWhitespace) | CharsWhile(!_.isWhitespace))).rep.!.map(t => Element.Unrecognised(t.trim)))

  private def main[_: P]: P[Iterator[Element]] =
    P((unrecognised ~ recognised).rep ~ unrecognised ~ End)
    .map(x => x._1.iterator.flatMap(y => y._1 :: y._2 :: Nil) ++ Iterator.single(x._2))

  // ===================================================================================================================

  private def justParse(t: String): List[Element] =
    fastparse.parse(t, main(_)) match {
      case Parsed.Success(value, _) =>
        value
          .filter {
            case Element.Empty            => false
            case _: Element.Success       => true
            case u: Element.Unrecognised  => u.text.nonEmpty
            case _: Element.AbstractClass => true
          }
          .toList
      case f: Parsed.Failure =>
        System.err.println(f.trace().longMsg)
        Element.Unrecognised(t) :: Nil
    }

  def parse(t: String): List[Element] = {
    val es = justParse(t)
    PostProcess(es)(_.success.map(_.value), Element.Success)
  }

  sealed trait Element {
    final def success: Option[Element.Success] = this match {
      case s: Element.Success => Some(s)
      case _: Element.Failure
         | Element.Empty      => None
    }
    final def failure: Option[Element.Failure] = this match {
      case f: Element.Failure => Some(f)
      case _: Element.Success
         | Element.Empty      => None
    }
  }
  object Element {
    sealed trait Failure extends Element
    case object Empty extends Element
    final case class Success(value: data.TypeDef) extends Element
    final case class Unrecognised(text: String) extends Failure
    final case class AbstractClass(value: Cls) extends Failure

    implicit def univEqU: UnivEq[Unrecognised] = UnivEq.derive
    implicit def univEq: UnivEq[Element] = UnivEq.derive
  }
}
