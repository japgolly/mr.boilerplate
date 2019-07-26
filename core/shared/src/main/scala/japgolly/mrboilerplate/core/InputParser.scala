package japgolly.mrboilerplate.core

import fastparse._
import fastparse.ScalaWhitespace._
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

    def ClsDef[_: P] = {
      def ClsAnnot = P( `@` ~ SimpleType ~ ArgList.? )
      def Prelude = P( NotNewline ~ ( ClsAnnot.rep(1) ~ AccessMod.? | AccessMod) )
      def ClsArgMod = P( Mod.rep ~ (`val` | `var`) )
      def ClsArg = P( Annot.rep ~ ClsArgMod.? ~ Id.! ~ `:` ~ Type.! ~ (`=` ~ ExprCtx.Expr).? )
      def ClsArgs = P( OneNLMax ~ "(" ~/ `implicit`.? ~ ClsArg.repTC() ~ ")" )

      P( `case`.? ~ `class` ~/ Id.! ~ TypeArgList2.? ~~ Prelude.? ~~ ClsArgs.repX ~ DefTmpl.? )
    }

    def Constrs[_: P] = P( (WL ~ Constr).rep(1, `with`./) )
    def EarlyDefTmpl[_: P] = P( TmplBody ~ (`with` ~/ Constr).rep ~ TmplBody.? )
    def NamedTmpl[_: P] = P( Constrs ~ TmplBody.? )

    def DefTmpl[_: P] = P( (`extends` | `<:`) ~ AnonTmpl | TmplBody )
    def AnonTmpl[_: P] = P( EarlyDefTmpl | NamedTmpl | TmplBody )

    def TraitDef[_: P] = P( `trait` ~/ Id ~ TypeArgList.? ~ DefTmpl.? )

    def ObjDef[_: P]: P[Unit] = P( `case`.? ~ `object` ~/ Id ~ DefTmpl.? )

    def Constr[_: P] = P( AnnotType ~~ (NotNewline ~ ParenArgList ).repX )

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

  private def cls[_: P]: P[Class] =
    P(Scala.Mod.? ~ Scala.ClsDef).map {
      case (name, types, fields) =>
        Class(
          name       = name,
          typeParams = types.iterator.flatten.map(Type(_)).toList,
          fields     = fields.iterator.take(1).flatten.map { case (n, t) => Field(FieldName(n), Type(t)) }.toList)
    }

  private def ignore[_: P]: P[Unit] =
    P(Scala.TopPkgSeq | Scala.Import | Scala.Literals.Comment)

  private def recognised[_: P]: P[Option[Class]] =
    P(cls.map(Some(_)) | ignore.map(_ => None))

  private def unrecognised[_: P]: P[Unrecognised] =
    P((!recognised ~~ AnyChar).repX.!.map(t => Unrecognised(t.trim)))

  type Element = Either[Unrecognised, Class]

  private def recognisedE[_: P]: P[Element] =
  // Left(Unrecognised("")) is a hack for Empty cos it will be filtered out below
    recognised.map(_.fold[Element](Left(Unrecognised("")))(Right(_)))

  private def unrecognisedE[_: P]: P[Element] =
    unrecognised.map(Left(_))

  private def main[_: P]: P[Iterator[Element]] =
    P((unrecognisedE ~ recognisedE).rep ~ unrecognisedE ~ End)
    .map(x => x._1.iterator.flatMap(y => y._1 :: y._2 :: Nil) ++ Iterator.single(x._2))

  def parse(t: String): List[Element] =
    fastparse.parse(t, main(_))
      .get
      .value
      .filterNot(_.left.exists(_.text.isEmpty))
      .toList

  final case class Unrecognised(text: String)
  implicit def univEqUnrecognised: UnivEq[Unrecognised] = UnivEq.derive
}