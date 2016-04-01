import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._
import Lib._

object TestState extends Build {

  private val ghProject = "test-state"

  object Ver {
    final val Scala211      = "2.11.8"
    final val Acyclic       = "0.1.4"
    final val MTest         = "0.4.3"
    final val MacroParadise = "2.1.0"
    final val KindProjector = "0.7.1"
    final val Nyaya         = "0.7.0"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
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
      scalacOptions in Test   --= Seq("-Ywarn-dead-code"),
      shellPrompt in ThisBuild := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage         := Watched.clearWhenTriggered,
      incOptions               := incOptions.value.withNameHashing(true),
      updateOptions            := updateOptions.value.withCachedResolution(true),
      addCompilerPlugin("org.spire-math" %% "kind-projector" % Ver.KindProjector))
    .configure(
      acyclicSettings,
      addCommandAliases(
        "/"   -> "project root",
        "L"   -> "root/publishLocal",
        "C"   -> "root/clean",
        "T"   -> ";root/clean;root/test",
        "TL"  -> ";T;L",
        "c"   -> "compile",
        "tc"  -> "test:compile",
        "t"   -> "test",
        "to"  -> "test-only",
        "cc"  -> ";clean;compile",
        "ctc" -> ";clean;test:compile",
        "ct"  -> ";clean;test"))

  def acyclicSettings: PE = _
    .settings(
      libraryDependencies += "com.lihaoyi" %% "acyclic" % Ver.Acyclic % "provided",
      addCompilerPlugin("com.lihaoyi" %% "acyclic" % Ver.Acyclic),
      autoCompilerPlugins := true)

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        // "org.scala-lang" % "scala-reflect" % Ver.Scala211,
        // "org.scala-lang" % "scala-library" % Ver.Scala211,
        "org.scala-lang" % "scala-compiler" % Ver.Scala211 % "provided"))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.full)

  def utestSettings: CPE = _
    .settings(
      libraryDependencies  += "com.lihaoyi" %%% "utest" % Ver.MTest,
      testFrameworks       += new TestFramework("utest.runner.Framework"))
    .jsSettings(
      scalaJSStage in Test := FastOptStage,
      jsEnv in Test        := NodeJSEnv().value)

  override def rootProject = Some(root)

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings, preventPublication)
      .aggregate(coreJVM, coreJS, coreMacrosJVM, coreMacrosJS)

  lazy val coreMacrosJVM = coreMacros.jvm
  lazy val coreMacrosJS  = coreMacros.js
  lazy val coreMacros = crossProject.in(file("core-macros"))
    .bothConfigure(commonSettings, publicationSettings(ghProject), definesMacros)
    .configure(utestSettings)

  lazy val coreJVM = core.jvm
  lazy val coreJS  = core.js
  lazy val core = crossProject
    .bothConfigure(commonSettings, publicationSettings(ghProject))
    .configure(utestSettings)
    .dependsOn(coreMacros)
    .settings(
      libraryDependencies ++= Seq(
        "com.github.japgolly.nyaya" %%% "nyaya-gen"  % Ver.Nyaya % "test",
        "com.github.japgolly.nyaya" %%% "nyaya-prop" % Ver.Nyaya % "test",
        "com.github.japgolly.nyaya" %%% "nyaya-test" % Ver.Nyaya % "test"))
}
