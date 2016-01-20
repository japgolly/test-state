import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._
import Lib._

object ScalaJsBenchmark extends Build {

  private val ghProject = "test-state"

  object Ver {
    final val Scala211 = "2.11.7"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-Ywarn-value-discard",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials")

  val commonSettings: PE =
    _.settings(
      organization             := "com.github.japgolly.test-state",
      version                  := "0.1.0-SNAPSHOT",
      homepage                 := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                 += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion             := Ver.Scala211,
      scalacOptions           ++= scalacFlags,
      shellPrompt in ThisBuild := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage         := Watched.clearWhenTriggered,
      incOptions               := incOptions.value.withNameHashing(true),
      updateOptions            := updateOptions.value.withCachedResolution(true))
    .configure(
      addCommandAliases(
        "/"   -> "project root",
        "C"   -> "root/clean",
        "T"   -> ";root/clean;root/test",
        "c"   -> "compile",
        "tc"  -> "test:compile",
        "t"   -> "test",
        "cc"  -> ";clean;compile",
        "ctc" -> ";clean;test:compile",
        "ct"  -> ";clean;test"))

  override def rootProject = Some(root)

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings, preventPublication)
      .aggregate(coreJVM, coreJS)

  lazy val core = crossProject
    .bothConfigure(commonSettings, publicationSettings(ghProject))

  lazy val coreJVM = core.jvm
  lazy val coreJS  = core.js
}
