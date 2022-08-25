import sbt._
import sbt.Keys._
import com.jsuereth.sbtpgp.PgpKeys
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import Lib._

object Build {

  private val ghProject = "mr.boilerplate"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    val BetterMonadicFor = "0.3.1"
    val Circe            = "0.14.2"
    val FastParse        = "2.3.3"
    val Microlibs        = "4.1.0"
    val Monocle          = "2.1.0"
    val MTest            = "0.8.0"
    val Scala2           = "2.13.8"
    val SJSReact         = "2.1.1"
    val UnivEq           = "2.0.0"
  }

  def byScalaVersion[A](f: PartialFunction[(Long, Long), Seq[A]]): Def.Initialize[Seq[A]] =
    Def.setting(CrossVersion.partialVersion(scalaVersion.value).flatMap(f.lift).getOrElse(Nil))

  def addMacroParadisePlugin = Def.settings(
    Seq(
      scalacOptions ++= byScalaVersion {
        case (2, 13) => Seq("-Ymacro-annotations")
      }.value
    ))

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-opt:l:inline",
    "-opt-inline-from:scala.**",
    "-opt-inline-from:japgolly.mrboilerplate.**",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard")

  val commonSettings = ConfigureBoth(
    _.settings(
      organization                  := "com.github.japgolly.mrboilerplate",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      startYear                     := Some(2019),
      scalaVersion                  := Ver.Scala2,
      scalacOptions                ++= scalacFlags,
      Test / scalacOptions         --= Seq("-Ywarn-dead-code"),
      testFrameworks                := Seq(new TestFramework("utest.runner.Framework")),
      incOptions                    := incOptions.value,
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(ThisBuild / version).value}",
      releaseVcsSign                := true,
      libraryDependencies ++= Seq(
        "com.lihaoyi"                   %%% "utest"     % Ver.MTest,
        "com.github.japgolly.microlibs" %%% "test-util" % Ver.Microlibs),
      addMacroParadisePlugin,
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % Ver.BetterMonadicFor))
      .configure(preventPublication))

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings.jvm)
      .aggregate(coreJVM, coreJS, webapp)

  lazy val coreJVM = core.jvm
  lazy val coreJS  = core.js
  lazy val core = crossProject(JSPlatform, JVMPlatform)
    .in(file("core"))
    .configureCross(commonSettings)
    .settings(
      moduleName := "core",
      libraryDependencies ++= Seq(
        "com.lihaoyi"                   %%% "fastparse"               % Ver.FastParse,
        "com.lihaoyi"                   %%% "scalaparse"              % Ver.FastParse,
        "com.github.japgolly.microlibs" %%% "adt-macros"              % Ver.Microlibs,
        "com.github.japgolly.microlibs" %%% "nonempty"                % Ver.Microlibs,
        "com.github.japgolly.microlibs" %%% "stdlib-ext"              % Ver.Microlibs,
        "com.github.japgolly.microlibs" %%% "utils"                   % Ver.Microlibs,
        "com.github.japgolly.univeq"    %%% "univeq"                  % Ver.UnivEq,
        "com.github.julien-truffaut"    %%% "monocle-core"            % Ver.Monocle,
        "com.github.julien-truffaut"    %%% "monocle-macro"           % Ver.Monocle,
      ),
    )

  lazy val webapp = project
    .in(file("webapp"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js)
    .dependsOn(coreJS)
    .settings(
      moduleName := "webapp",
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "core"               % Ver.SJSReact,
        "com.github.japgolly.scalajs-react" %%% "extra"              % Ver.SJSReact,
        "com.github.japgolly.scalajs-react" %%% "extra-ext-monocle2" % Ver.SJSReact,
        "io.circe"                          %%% "circe-core"         % Ver.Circe,
        "io.circe"                          %%% "circe-parser"       % Ver.Circe),
      scalaJSLinkerConfig ~= { _.withSourceMap(true) },
      Compile / fastOptJS / artifactPath := (crossTarget.value / "mr-boilerplate.js"),
      Compile / fullOptJS / artifactPath := (crossTarget.value / "mr-boilerplate.js"))
}
