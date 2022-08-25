package japgolly.mrboilerplate.core

import fastparse._
import japgolly.mrboilerplate.core.data._
import japgolly.univeq.UnivEq
import scala.annotation.nowarn

@nowarn("msg=Top-level wildcard is not allowed")
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

    import fastparse.ScalaWhitespace._

    def TypeArg2[_](implicit p: P[Any]): P[Type] = {
      def CtxBounds = P((`<%` ~/ Type).rep ~ (`:` ~/ Type).rep)
      P(((Id | `_`) ~ TypeArgList.?).!.map(s => data.Type(s.trim)) ~ TypeBounds ~ CtxBounds)
    }

    def TypeArgList2[_](implicit p: P[Any]): P[Seq[Type]] = {
      def Variant: P[Type] = P( Annot.rep ~ CharIn("+\\-").? ~ TypeArg2 )
      P( "[" ~/ Variant.repTC(1) ~ "]" )
    }

    def TmplBody[_](implicit p: P[Any]): P[Unit] = {
      def Prelude = P( (Annot ~ OneNLMax).rep ~ Mod./.rep )
      // def TmplStat = P( Import | (Prelude ~ BlockDef) | StatCtx.Expr )
      def TmplStat = P(Import | Prelude ~ (Dcl | ObjDef) | StatCtx.Expr)

      P( "{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = NoCut(Semis)) ~ Semis.? ~ `}` )
    }

    override def ValVarDef[_](implicit p: P[Any]) =
      P( BindPattern.rep(1, ","./) ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr).? )

    override def FunDef[_](implicit p: P[Any]) = {
      def Body = P( WL ~ `=` ~/ `macro`.? ~ StatCtx.Expr | OneNLMax ~ "{" ~ Block ~ "}" )
      P( FunSig ~ (`:` ~/ Type).? ~~ Body.? )
    }

    override def BlockDef[_](implicit p: P[Any]): P[Unit] =
      P( Dcl | TraitDef  | ClsDef | ObjDef )

    def ClsDef[_](implicit p: P[Any]): P[(String, Option[Seq[Type]], Seq[Seq[(String, String)]], Seq[Type])] = {
      def ClsAnnot = P( `@` ~ SimpleType ~ ArgList.? )
      def Prelude = P( NotNewline ~ ( ClsAnnot.rep(1) ~ AccessMod.? | AccessMod) )
      def ClsArgMod = P( Mod.rep ~ (`val` | `var`) )
      def ClsArg = P( Annot.rep ~ ClsArgMod.? ~ Id.! ~ `:` ~ Type.! ~ (`=` ~ ExprCtx.Expr).? )
      def ClsArgs = P( OneNLMax ~ "(" ~/ `implicit`.? ~ ClsArg.repTC() ~ ")" )

      P( `case`.? ~ `class` ~/ Id.! ~ TypeArgList2.? ~~ Prelude.? ~~ ClsArgs.repX ~ DefTmpl_? )
    }

    def Constrs[_](implicit p: P[Any]): P[Seq[data.Type]] =
      P( (WL ~ Constr).rep(1, `with`./) )

    def EarlyDefTmpl[_](implicit p: P[Any]): P[Seq[data.Type]] =
      P( TmplBody ~ (`with` ~/ Constr).rep ~ TmplBody.? )

    def NamedTmpl[_](implicit p: P[Any]): P[Seq[data.Type]] =
      P( Constrs ~ TmplBody.? )

    def DefTmpl[_](implicit p: P[Any]): P[Seq[data.Type]] =
      P( (`extends` | `<:`) ~ AnonTmpl2 | TmplBody.map(_ => Seq.empty) )

    def DefTmpl_?[_](implicit p: P[Any]): P[Seq[data.Type]] =
      P(DefTmpl.?.map(_.getOrElse(Seq.empty)))

    def AnonTmpl2[_](implicit p: P[Any]): P[Seq[data.Type]] =
      P( EarlyDefTmpl | NamedTmpl | TmplBody.map(_ => Seq.empty) )

    override def AnonTmpl[_](implicit p: P[Any]) = AnonTmpl2.map(_ => ())

    def TraitDef[_](implicit p: P[Any]): P[(String, Option[Seq[Type]], Seq[Type])] =
      P( `trait` ~/ Id.! ~ TypeArgList2.? ~ DefTmpl_? )

    def ObjDef[_](implicit p: P[Any]): P[Unit] = P( `case`.? ~ `object` ~/ Id ~ DefTmpl.? )
    def ObjDef2[_](implicit p: P[Any]) = P( `case`.!.? ~ `object` ~/ Id.! ~ DefTmpl_? )

    def Constr[_](implicit p: P[Any]) = P( AnnotType2 ~~ (NotNewline ~ ParenArgList ).repX )
    def AnnotType2[_](implicit p: P[Any]) = P(SimpleType.!.map(s => data.Type(s.trim)) ~~ NLAnnot.repX )

//    def PkgObj[_](implicit p: P[Any]) = P( ObjDef )
//    def PkgBlock[_](implicit p: P[Any]) = P( QualId ~/ `{` ~ TopStatSeq.? ~ `}` )
//    def Pkg[_](implicit p: P[Any]) = P( `package` ~/ (PkgBlock | PkgObj) )
//    def TopStatSeq[_](implicit p: P[Any]): P[Unit] = {
//      def Tmpl = P( (Annot ~~ OneNLMax).rep ~ Mod.rep ~ (TraitDef | ClsDef | ObjDef) )
//      def TopStat = P( Pkg | Import | Tmpl )
//      P( TopStat.repX(1, Semis) )
//    }
    def TopPkgSeq[_](implicit p: P[Any]) = P( ((`package` ~ QualId) ~~ !(WS ~ "{")).repX(1, Semis) )
//    def CompilationUnit[_](implicit p: P[Any]): P[Unit] = {
//      def Body = P( TopPkgSeq ~~ (Semis ~ TopStatSeq).? | TopStatSeq )
//      P( Semis.? ~ Body.? ~~ Semis.? ~ WL0 ~ End )
//    }
  }

  // ===================================================================================================================

  import fastparse.MultiLineWhitespace._

  private def flags[_](implicit p: P[Any]): P[Flags] =
    P(Scala.Mod.rep.!.map(Flags.fromStr))

  private def cls[_](implicit p: P[Any]): P[Element] =
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

  private def sealedTrait[_](implicit p: P[Any]): P[Element] =
    P(flags.filter(_.isSealed) ~ Scala.TraitDef).map {
      case (_, (name, types, sup)) =>
        Element.Success(SealedBase(
          name           = name,
          typeParams     = types.iterator.flatten.toList,
          superTypes     = sup.toList,
          directChildren = Nil,
        ))
    }

  private def obj[_](implicit p: P[Any]): P[Element] =
    P(Scala.Mod.rep ~ Scala.ObjDef2)
      .filter(x => x._1.isDefined || x._3.nonEmpty)
      .map {
        case (_, name, sup) =>
          Element.Success(Obj(
            name       = name,
            superTypes = sup.toList,
          ))
      }

  private def ignore[_](implicit p: P[Any]): P[Unit] =
    P(Scala.TopPkgSeq | Scala.Import | Scala.Literals.Comment | Scala.Annot | (Scala.Mod.rep ~ Scala.Dcl))

  private def recognised[_](implicit p: P[Any]): P[Element] =
    P(obj | cls | sealedTrait | ignore.rep(1).map(_ => Element.Empty))

  private def unrecognised[_](implicit p: P[Any]): P[Element] =
    P((!recognised ~~ (CharPred(_.isWhitespace) | CharsWhile(!_.isWhitespace))).rep.!.map(Element.Unrecognised(_)))

  private def main[_](implicit p: P[Any]): P[Iterator[Element]] =
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

    object Unrecognised {
      def apply(s: String): Unrecognised = {
        // "Performance"? Never heard of it!
        val t = s.trim.split("\n").reverse.dropWhile(_.matches("^[\\s}]*$")).reverse.mkString("\n").trim
        new Unrecognised(t)
      }
    }

    implicit def univEqU: UnivEq[Unrecognised] = UnivEq.derive
    implicit def univEqF: UnivEq[Failure] = UnivEq.derive
    implicit def univEqS: UnivEq[Success] = UnivEq.derive
    implicit def univEq: UnivEq[Element] = UnivEq.derive
  }
}
