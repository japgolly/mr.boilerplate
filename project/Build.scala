import sbt._
import sbt.Keys._
import com.typesafe.sbt.pgp.PgpKeys
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{crossProject => _, CrossType => _, _}
import sbtcrossproject.CrossPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import Lib._

object Build {

  private val ghProject = "mr.boilerplate"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    final val BetterMonadicFor = "0.3.1"
    final val Microlibs        = "1.22"
    final val MTest            = "0.6.9"
    final val Scala212         = "2.12.8"
    final val SJSReact         = "1.4.2"
    final val UnivEq           = "1.0.8"
  }

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
      scalaVersion                  := Ver.Scala212,
      scalacOptions                ++= scalacFlags,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code"),
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage              := Watched.clearWhenTriggered,
      incOptions                    := incOptions.value,
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % Ver.BetterMonadicFor))
      .configure(preventPublication))

  def utestSettings = ConfigureBoth(
    _.settings(
      testFrameworks      += new TestFramework("utest.runner.Framework"),
      libraryDependencies ++= Seq(
        "com.lihaoyi"                   %%% "utest"     % Ver.MTest     % Test,
        "com.github.japgolly.microlibs" %%% "test-util" % Ver.Microlibs % Test)))

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings.jvm)
      .aggregate(coreJVM, coreJS, webapp)

  lazy val coreJVM = core.jvm
  lazy val coreJS  = core.js
  lazy val core = crossProject(JSPlatform, JVMPlatform)
    .in(file("core"))
    .configureCross(commonSettings, utestSettings)
    .settings(
      moduleName := "core",
      libraryDependencies ++= Seq(
        "com.github.japgolly.microlibs" %%% "stdlib-ext" % Ver.Microlibs,
        "com.github.japgolly.microlibs" %%% "utils"      % Ver.Microlibs,
        "com.github.japgolly.univeq"    %%% "univeq"     % Ver.UnivEq))

  lazy val webapp = project
    .in(file("webapp"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, utestSettings.js)
    .dependsOn(coreJS)
    .settings(
      moduleName := "webapp",
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "extra" % Ver.SJSReact))
}
