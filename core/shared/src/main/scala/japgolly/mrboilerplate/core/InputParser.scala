package japgolly.mrboilerplate.core

import fastparse._
import fastparse.MultiLineWhitespace._
import japgolly.mrboilerplate.core.data._
import japgolly.univeq.UnivEq

object InputParser {

  private object Scala
    extends scalaparse.Core
      with scalaparse.Types
      with scalaparse.Exprs {

    def TypeArg2[_: P]: P[String] = {
      def CtxBounds = P((`<%` ~/ Type).rep ~ (`:` ~/ Type).rep)
      P(((Id | `_`) ~ TypeArgList.?).! ~ TypeBounds ~ CtxBounds)
    }

    def TypeArgList2[_: P]: P[Seq[String]] = {
      def Variant: P[String] = P( Annot.rep ~ CharIn("+\\-").? ~ TypeArg2 )
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

    def ClsDef[_: P]: P[(String, Option[Seq[String]], Seq[Seq[(String, String)]], Seq[Type])] = {
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

    def TraitDef[_: P] = P( `trait` ~/ Id ~ TypeArgList.? ~ DefTmpl.? )

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

  private def cls[_: P]: P[Element] =
    P(Scala.Mod.rep ~ Scala.ClsDef).map {
      case (name, types, fields, sup) =>
        Element.Class(Cls(
          name       = name,
          typeParams = types.iterator.flatten.map(Type(_)).toList,
          fields     = fields.iterator.take(1).flatten.map { case (n, t) => Field(FieldName(n), Type(t)) }.toList,
          superTypes = sup.toList,
        ))
    }

  private def ignore[_: P]: P[Unit] =
    P(Scala.TopPkgSeq | Scala.Import | Scala.Literals.Comment)

  private def recognised[_: P]: P[Element] =
    P(cls | ignore.rep(1).map(_ => Element.Empty))
//    P(cls | ignore.rep(1).!.map{s =>
//      println(s"Ignoring: [${s.replace("\n", "\\n")}]")
//      Element.Empty})

  private def unrecognised[_: P]: P[Element] =
    P((!recognised ~~ (CharPred(_.isWhitespace) | CharsWhile(!_.isWhitespace))).rep.!.map(t => Element.Unrecognised(t.trim)))

  private def main[_: P]: P[Iterator[Element]] =
    P((unrecognised ~ recognised).rep ~ unrecognised ~ End)
    .map(x => x._1.iterator.flatMap(y => y._1 :: y._2 :: Nil) ++ Iterator.single(x._2))

  // ===================================================================================================================

  def parse(t: String): List[Element] =
    fastparse.parse(t, main(_)) match {
      case Parsed.Success(value, _) =>
        value
          .filter {
            case Element.Empty           => false
            case _: Element.Class        => true
            case u: Element.Unrecognised => u.text.nonEmpty
          }
          .toList
      case f: Parsed.Failure =>
        System.err.println(f.trace().longMsg)
        Element.Unrecognised(t) :: Nil
    }

  sealed trait Element {
    final def success: Option[Cls] = this match {
      case Element.Class(cls)        => Some(cls)
      case Element.Unrecognised(_)
         | Element.Empty             => None
    }
    final def failure: Option[Element.Unrecognised] = this match {
      case u: Element.Unrecognised => Some(u)
      case _: Element.Class
         | Element.Empty           => None
    }
  }
  object Element {
    case object Empty extends Element
    final case class Unrecognised(text: String) extends Element
    final case class Class(value: Cls) extends Element

    implicit def univEqU: UnivEq[Unrecognised] = UnivEq.derive
    implicit def univEq: UnivEq[Element] = UnivEq.derive
  }
}
