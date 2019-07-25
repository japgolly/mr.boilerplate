package japgolly.mrboilerplate.core

import fastparse._
import fastparse.ScalaWhitespace._

object InputParser {

  //  def ClsDef[_: P] = {
//    def ClsAnnot = P( `@` ~ SimpleType ~ ArgList.? )
//    def Prelude = P( NotNewline ~ ( ClsAnnot.rep(1) ~ AccessMod.? | AccessMod) )
//    def ClsArgMod = P( Mod.rep ~ (`val` | `var`) )
//    def ClsArg = P( Annot.rep ~ ClsArgMod.? ~ Id ~ `:` ~ Type ~ (`=` ~ ExprCtx.Expr).? )
//
//    def ClsArgs = P( OneNLMax ~ "(" ~/ `implicit`.? ~ ClsArg.repTC() ~ ")" )
//    P( `case`.? ~ `class` ~/ Id ~ TypeArgList.? ~~ Prelude.? ~~ ClsArgs.repX ~ DefTmpl.? )
//  }

  private object Scala
    extends scalaparse.Core
      with scalaparse.Types
      with scalaparse.Exprs {

    def TypeArgList2[_: P]: P[Seq[String]] = {
      def Variant: P[String] = P( Annot.rep ~ CharIn("+\\-").? ~ TypeArg.! )
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
//    def TopPkgSeq[_: P] = P( ((`package` ~ QualId) ~~ !(WS ~ "{")).repX(1, Semis) )
//    def CompilationUnit[_: P]: P[Unit] = {
//      def Body = P( TopPkgSeq ~~ (Semis ~ TopStatSeq).? | TopStatSeq )
//      P( Semis.? ~ Body.? ~~ Semis.? ~ WL0 ~ End )
//    }
  }

  // ===================================================================================================================

  private def cls[_: P]: P[Class] =
    P(Scala.ClsDef).map {
      case (name, types, fields) =>
        Class(
          name       = name,
          typeParams = types.iterator.flatten.map(Type(_)).toList,
          fields     = fields.iterator.take(1).flatten.map { case (n, t) => Field(FieldName(n), Type(t)) }.toList)
    }

  private def unrecognised[_: P]: P[Unit] =
    P((!cls ~~ AnyChar).repX)

  private def main[_: P]: P[Seq[Class]] =
    P((unrecognised ~ cls).rep ~ unrecognised ~ End)

  def parseText(t: String): Seq[Class] =
    parse(t, main(_)).get.value // TODO .get
}
