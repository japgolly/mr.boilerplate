package japgolly.mrboilerplate.webapp

import io.circe._
import io.circe.parser.parse
import io.circe.syntax._
import japgolly.mrboilerplate.core.gen._
import japgolly.scalajs.react.{Callback, CallbackTo}
import monocle.Lens
import org.scalajs.dom.raw.Storage
import scala.scalajs.js

object PersistentState {
  import MainComponent.State

  trait Field {
    type S
    type A
    val key: String
    val lens: Lens[S, A]
    val decoder: Decoder[A]
    val encoder: Encoder[A]

    final def zoomOut[SS](lens: Lens[SS, S]) =
      Field(key, lens ^|-> this.lens)(decoder, encoder)

    final def mapKey(f: String => String) =
      Field(f(key), lens)(decoder, encoder)

    final def scope[SS](key: String, lens: Lens[SS, S]) =
      mapKey(key + "." + _).zoomOut(lens)
  }

  object Field {
    type From[X] = Field { type S = X }

    def apply[X, Y](_key: String,
                    _lens: Lens[X, Y])
                   (implicit dec: Decoder[Y], enc: Encoder[Y]): Field { type S = X; type A = Y } =
      new Field {
        override type S = X
        override type A = Y
        override val key = _key
        override val lens = _lens
        override val decoder = dec
        override val encoder = enc
      }

    def scope[S, T](key: String, lens: Lens[S, T])(fs: From[T]*): List[From[S]] =
      fs.iterator.map(_.scope(key, lens)).toList
  }

  // ===================================================================================================================

  private def decodeSumBySoleKey[A](f: PartialFunction[(String, ACursor), Decoder.Result[A]]): Decoder[A] = {
    def keyErr = "Expected a single key indicating the subtype"
    Decoder.instance { c =>
      c.keys match {
        case Some(it) =>
          it.toList match {
            case singleKey :: Nil =>
              val arg  = (singleKey, c.downField(singleKey))
              def fail = Left(DecodingFailure("Unknown subtype: " + singleKey, c.history))
              f.applyOrElse(arg, (_: (String, ACursor)) => fail)
            case Nil  => Left(DecodingFailure(keyErr, c.history))
            case keys => Left(DecodingFailure(s"$keyErr, found multiple: $keys", c.history))
          }
        case None => Left(DecodingFailure(keyErr, c.history))
      }
    }
  }

  val fields: List[Field.From[State]] = {

    implicit val decoderCirceOptionsSumTypeFormat: Decoder[Circe.Options.SumTypeFormat] = decodeSumBySoleKey {
      case ("TypeToValue"  , _) => Right(Circe.Options.SumTypeFormat.TypeToValue)
      case ("UntaggedUnion", _) => Right(Circe.Options.SumTypeFormat.UntaggedUnion)
    }

    implicit val encoderCirceOptionsSumTypeFormat: Encoder[Circe.Options.SumTypeFormat] = Encoder.instance {
      case a@ Circe.Options.SumTypeFormat.TypeToValue   => Json.obj("TypeToValue"   -> ().asJson)
      case a@ Circe.Options.SumTypeFormat.UntaggedUnion => Json.obj("UntaggedUnion" -> ().asJson)
    }

    def genOptions(key: String, gd: GeneratorDef)(f: gd.gen.Options => List[Field.From[gd.gen.Options]]): List[Field.From[State]] = {
      type FS = Field.From[GeneratorsComponent.State]

      // this is purely so that when new fields are added, this file must be ack'd
      val opt: gd.gen.Options =
        Default.options(gd)

      val optFields: List[FS] =
        f(opt).map(_.zoomOut(GeneratorsComponent.State.genOptions(gd)))

      val enabledField: FS =
        Field("enabled", GeneratorsComponent.State.genEnabled(gd))

      val fields: List[FS] =
        enabledField :: optFields

      fields.map(_.scope(s"gen.$key", State.gen))
    }

    def glopts(f: GlobalOptions => List[Field.From[GlobalOptions]]): List[Field.From[State]] = {
      val lens = State.gen ^|-> GeneratorsComponent.State.glopt
      f(Default.globalOptions).map(_.scope("glopt", lens))
    }

    val input = Field.scope("input", State.input)(
      Field("typePrefix", InputComponent.State.typePrefix),
    )

    val circe = genOptions("circe", GeneratorDef.CirceDef) {
      case Circe.Options(_, _, _, _) => List[Field.From[Circe.Options]](
        Field("keyConstants"    , Circe.Options.keyConstants),
        Field("monadicObjects"  , Circe.Options.monadicObjects),
        Field("singlesAsObjects", Circe.Options.singlesAsObjects),
        Field("sumTypes"        , Circe.Options.sumTypes),
      )
    }

    val boopickle = genOptions("boopickle", GeneratorDef.BooPickleDef) {
      case BooPickle.Options(_, _, _) => List(
        Field("conciseSingleFields", BooPickle.Options.conciseSingleFields),
        Field("keyConstants"       , BooPickle.Options.keyConstants),
        Field("objectCodecs"       , BooPickle.Options.objectCodecs),
      )
    }

    val univeq = genOptions("univeq", GeneratorDef.UnivEqDef) {
      case UnivEqGen.Options(_) => List(
        Field("oneLine", UnivEqGen.Options.oneLine)
      )
    }

    val globalOptions = glopts {
      case GlobalOptions(_, _, _) => List(
        Field("generateCompanions", GlobalOptions.generateCompanions),
        Field("makeValsLazy"      , GlobalOptions.makeValsLazy),
        Field("generateCompanions", GlobalOptions.generateCompanions),
      )
    }

    val b = List.newBuilder[Field.From[State]]
    b ++= input
    b ++= circe
    b ++= boopickle
    b ++= univeq
    b ++= globalOptions
    b.result()
  }

  // ===================================================================================================================

  def fromJson(j: Json)(s0: State): State = {
    val jc = j.hcursor
    var s = s0
    for {
      f <- fields
      a <- jc.downField(f.key).as(f.decoder).toOption
    } s = f.lens.set(a)(s)
    s
  }

  def toJson(s: State): Json =
    Json.obj(fields.map(f => (f.key, f.encoder(f.lens.get(s)))): _*)

  // ===================================================================================================================

  private val storage: CallbackTo[Option[Storage]] =
    CallbackTo(
      Option {
        js.UndefOr.any2undefOrA(org.scalajs.dom.window.localStorage).orNull
      }
    )

  private[this] final val storageKey = "state"

  def write(state: State): Callback = {
    storage.map(_.foreach { s =>
      val json = toJson(state).noSpaces
      // org.scalajs.dom.console.log("Writing: ", js.JSON.parse(json))
      s.setItem(storageKey, json)
    })
  }.attempt.void

  val read: CallbackTo[Option[State => State]] = {
    storage.map(storageOption =>
      for {
        s <- storageOption
        v <- Option(s.getItem(storageKey))
        j <- parse(v).toOption
      } yield {
        // org.scalajs.dom.console.log("Reading: ", js.JSON.parse(j.noSpaces))
        fromJson(j)(_)
      }
    ).attempt.map(_.toOption.flatten)
  }
}
