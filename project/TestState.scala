import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._
import Lib._

object TestState {

  private val ghProject = "test-state"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    final val Acyclic       = "0.1.4"
    final val Cats          = "0.5.0"
    final val KindProjector = "0.7.1"
    final val MacroParadise = "2.1.0"
    final val MTest         = "0.4.3"
    final val Nyaya         = "0.7.0"
    final val Scala211      = "2.11.8"
    final val ScalaJsDom    = "0.9.0"
    final val ScalaJsReact  = "0.11.1"
    final val Scalaz        = "7.2.1"
    final val Sizzle        = "2.3.0"
    final val UnivEq        = "1.0.0"

    // Used in examples only
    final val Monocle       = "1.2.1"
    final val ReactJs       = "15.0.1"
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

  val commonSettings = ConfigureBoth(
    _.settings(
      organization             := "com.github.japgolly.test-state",
      version                  := "2.0.0-SNAPSHOT",
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
        "tq"  -> "test-quick",
        "cc"  -> ";clean;compile",
        "ctc" -> ";clean;test:compile",
        "ct"  -> ";clean;test")))
    .jsConfigure(
      _.settings(scalaJSUseRhino := false))

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

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % "test",
      testFrameworks      += new TestFramework("utest.runner.Framework")))
    .jsConfigure(
      // Not mandatory; just faster.
      _.settings(jsEnv in Test := PhantomJSEnv().value))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(rootJVM, rootJS, examples)

  lazy val rootJVM =
    Project("JVM", file(".rootJVM"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        coreJVM, coreMacrosJVM, scalazJVM, catsJVM, nyayaJVM)

  lazy val rootJS =
    Project("JS", file(".rootJS"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        coreJS, coreMacrosJS, scalazJS, catsJS, nyayaJS,
        domZipperJS, domZipperSizzleJS,
        scalajsReactJS)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val coreMacrosJVM = coreMacros.jvm
  lazy val coreMacrosJS  = coreMacros.js
  lazy val coreMacros = crossProject
    .in(file("core-macros"))
    .configure(commonSettings)
    .bothConfigure(publicationSettings, definesMacros)
    .configure(utestSettings)

  lazy val coreJVM = core.jvm
  lazy val coreJS  = core.js
  lazy val core = crossProject
    .configure(commonSettings)
    .bothConfigure(publicationSettings)
    .dependsOn(coreMacros)
    .configure(utestSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.github.japgolly.univeq" %%% "univeq"     % Ver.UnivEq,
        "com.github.japgolly.nyaya"  %%% "nyaya-gen"  % Ver.Nyaya % "test",
        "com.github.japgolly.nyaya"  %%% "nyaya-prop" % Ver.Nyaya % "test",
        "com.github.japgolly.nyaya"  %%% "nyaya-test" % Ver.Nyaya % "test"))

  lazy val domZipperJS = project
    .in(file("dom-zipper"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, publicationSettings, utestSettings.js)
    .settings(
      moduleName          := "dom-zipper",
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % Ver.ScalaJsDom,
      requiresDOM         := true)

  lazy val domZipperSizzleJS = project
    .in(file("dom-zipper-sizzle"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, publicationSettings)
    .dependsOn(domZipperJS)
    .settings(
      moduleName     := "dom-zipper-sizzle",
      scalacOptions  -= "-Ywarn-dead-code",
      jsDependencies += "org.webjars.bower" % "sizzle" % Ver.Sizzle / "sizzle.min.js" commonJSName "Sizzle",
      requiresDOM    := true)

  lazy val scalazJVM = scalaz.jvm
  lazy val scalazJS  = scalaz.js
  lazy val scalaz = crossProject
    .in(file("ext-scalaz"))
    .configure(commonSettings)
    .bothConfigure(publicationSettings)
    .dependsOn(core)
    .configure(utestSettings)
    .settings(libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val catsJVM = cats.jvm
  lazy val catsJS  = cats.js
  lazy val cats = crossProject
    .in(file("ext-cats"))
    .configure(commonSettings)
    .bothConfigure(publicationSettings)
    .dependsOn(core)
    .configure(utestSettings)
    .settings(libraryDependencies += "org.typelevel" %%% "cats" % Ver.Cats)

  lazy val nyayaJVM = nyaya.jvm
  lazy val nyayaJS  = nyaya.js
  lazy val nyaya = crossProject
    .in(file("ext-nyaya"))
    .configure(commonSettings)
    .bothConfigure(publicationSettings)
    .dependsOn(core, scalaz)
    .configure(utestSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.github.japgolly.nyaya" %%% "nyaya-gen" % Ver.Nyaya,
        "com.github.japgolly.nyaya" %%% "nyaya-test" % Ver.Nyaya))

  lazy val scalajsReactJS = project
    .in(file("ext-scalajs-react"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, publicationSettings)
    .dependsOn(domZipperJS)
    .settings(
      moduleName := "scalajs-react",
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "core" % Ver.ScalaJsReact,
        "com.github.japgolly.scalajs-react" %%% "test" % Ver.ScalaJsReact),
      requiresDOM := true)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val examples =
    Project("examples", file(".examples"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(exampleReactJS)

  lazy val exampleReactJS = project
    .in(file("example-react"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, preventPublication, utestSettings.js)
    .dependsOn(coreJS, scalajsReactJS, domZipperSizzleJS)
    .settings(
      moduleName := "example-react",
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "ext-monocle"   % Ver.ScalaJsReact,
        "com.github.julien-truffaut"        %%% "monocle-core"  % Ver.Monocle,
        "com.github.julien-truffaut"        %%% "monocle-macro" % Ver.Monocle),
      addCompilerPlugin(macroParadisePlugin), // For Monocle macros
      jsDependencies ++= Seq(
        "org.webjars.bower" % "react" % Ver.ReactJs
          /        "react-with-addons.js"
          minified "react-with-addons.min.js"
          commonJSName "React",
        "org.webjars.bower" % "react" % Ver.ReactJs
          /         "react-dom.js"
          minified  "react-dom.min.js"
          dependsOn "react-with-addons.js"
          commonJSName "ReactDOM",
        "org.webjars.bower" % "react" % Ver.ReactJs
          /         "react-dom-server.js"
          minified  "react-dom-server.min.js"
          dependsOn "react-dom.js"
          commonJSName "ReactDOMServer"),
      requiresDOM := true)
}
