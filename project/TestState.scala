import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import Lib._

object TestState {

  private val ghProject = "test-state"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    final val Acyclic         = "0.1.7"
    final val Cats            = "1.1.0"
    final val KindProjector   = "0.9.6"
    final val MacroParadise   = "2.1.1"
    final val Microlibs       = "1.14"
    final val MTest           = "0.4.8"
    final val Nyaya           = "0.8.1"
    final val Scala211        = "2.11.12"
    final val Scala212        = "2.12.4"
    final val ScalaJsDom      = "0.9.5"
    final val ScalaJsReact    = "1.2.0"
    final val ScalaJsJavaTime = "0.2.4"
    final val Scalaz          = "7.2.21"
    final val Selenium        = "3.11.0"
    final val Sizzle          = "2.3.0"
    final val UnivEq          = "1.0.2"

    // Used in examples only
    final val Monocle       = "1.5.0"
    final val ReactJs       = "16.2.0"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-Ywarn-dead-code",
    // "-Ywarn-unused",
    "-Ywarn-value-discard",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials")

  val commonSettings = ConfigureBoth(
    _.settings(
      organization              := "com.github.japgolly.test-state",
      homepage                  := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                  += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion              := Ver.Scala212,
      crossScalaVersions        := Seq(Ver.Scala211, Ver.Scala212),
      scalacOptions            ++= scalacFlags,
      scalacOptions in Compile ++= byScalaVersion { case (2, 12) => Seq("-opt:l:method") }.value,
      scalacOptions in Test    --= Seq("-Ywarn-dead-code"),
      shellPrompt in ThisBuild  := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage          := Watched.clearWhenTriggered,
      incOptions                := incOptions.value.withNameHashing(true).withLogRecompileOnMacro(false),
      updateOptions             := updateOptions.value.withCachedResolution(true),
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

  def byScalaVersion[A](f: PartialFunction[(Int, Int), Seq[A]]): Def.Initialize[Seq[A]] =
    Def.setting(CrossVersion.partialVersion(scalaVersion.value).flatMap(f.lift).getOrElse(Nil))

  def acyclicSettings: PE = _
    .settings(
      libraryDependencies += "com.lihaoyi" %% "acyclic" % Ver.Acyclic % Provided,
      addCompilerPlugin("com.lihaoyi" %% "acyclic" % Ver.Acyclic),
      autoCompilerPlugins := true)

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        // "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.full)

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % Test,
      testFrameworks      += new TestFramework("utest.runner.Framework")))
    .jsConfigure(
      _.settings(jsEnv in Test := new JSDOMNodeJSEnv))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(rootJVM, rootJS, examples)

  lazy val rootJVM =
    Project("JVM", file(".rootJVM"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        coreJVM, coreMacrosJVM,
        domZipperJVM, domZipperSelenium,
        extScalazJVM, extCatsJVM, extNyayaJVM)

  lazy val rootJS =
    Project("JS", file(".rootJS"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        coreJS, coreMacrosJS,
        domZipperJS, domZipperSizzle,
        extScalazJS, extCatsJS, extNyayaJS, extScalaJsReact)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val coreMacrosJVM = coreMacros.jvm
  lazy val coreMacrosJS  = coreMacros.js
  lazy val coreMacros = crossProject
    .in(file("core-macros"))
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .bothConfigure(definesMacros)
    .settings(moduleName := "core-macros")

  lazy val coreJVM = core.jvm
  lazy val coreJS  = core.js
  lazy val core = crossProject
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(coreMacros)
    .configureCross(utestSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.github.japgolly.univeq" %%% "univeq"     % Ver.UnivEq,
        "com.github.japgolly.nyaya"  %%% "nyaya-gen"  % Ver.Nyaya % "test",
        "com.github.japgolly.nyaya"  %%% "nyaya-prop" % Ver.Nyaya % "test",
        "com.github.japgolly.nyaya"  %%% "nyaya-test" % Ver.Nyaya % "test"))
    .jsSettings(
      libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % Ver.ScalaJsJavaTime)

  lazy val domZipperJVM = domZipper.jvm
  lazy val domZipperJS  = domZipper.js
  lazy val domZipper = crossProject
    .in(file("dom-zipper"))
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(moduleName := "dom-zipper")
    .jsSettings(
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % Ver.ScalaJsDom,
      jsEnv               := new JSDOMNodeJSEnv)

  lazy val domZipperSelenium = project
    .in(file("dom-zipper-selenium"))
    .configure(commonSettings.jvm, publicationSettings.jvm, utestSettings.jvm)
    .dependsOn(domZipperJVM)
    .settings(
      moduleName := "dom-zipper-selenium",
      libraryDependencies ++= Seq(
        "org.seleniumhq.selenium"        % "selenium-api"            % Ver.Selenium,
        "org.seleniumhq.selenium"        % "selenium-chrome-driver"  % Ver.Selenium % Test,
        "org.seleniumhq.selenium"        % "selenium-firefox-driver" % Ver.Selenium % Test,
        "com.github.japgolly.microlibs" %% "test-util"               % Ver.Microlibs % Test),
      fork in Test := true,
      javaOptions in Test ++= Seq(
        "-Dsbt.baseDirectory=" + baseDirectory.value.getAbsolutePath,
        "-DCI=" + System.getProperty("CI", "")))

  lazy val domZipperSizzle = project
    .in(file("dom-zipper-sizzle"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, publicationSettings.js, utestSettings.js)
    .dependsOn(domZipperJS)
    .settings(
      moduleName     := "dom-zipper-sizzle",
      scalacOptions  -= "-Ywarn-dead-code",
      jsDependencies += "org.webjars.bower" % "sizzle" % Ver.Sizzle / "sizzle.min.js" commonJSName "Sizzle",
      jsEnv          := new JSDOMNodeJSEnv)

  lazy val extScalazJVM = extScalaz.jvm
  lazy val extScalazJS  = extScalaz.js
  lazy val extScalaz = crossProject
    .in(file("ext-scalaz"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(core)
    .configureCross(utestSettings)
    .settings(
      moduleName          := "ext-scalaz",
      libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val extCatsJVM = extCats.jvm
  lazy val extCatsJS  = extCats.js
  lazy val extCats = crossProject
    .in(file("ext-cats"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(core)
    .configureCross(utestSettings)
    .settings(
      moduleName          := "ext-cats",
      libraryDependencies += "org.typelevel" %%% "cats-core" % Ver.Cats)

  lazy val extNyayaJVM = extNyaya.jvm
  lazy val extNyayaJS  = extNyaya.js
  lazy val extNyaya = crossProject
    .in(file("ext-nyaya"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(core, extScalaz)
    .configureCross(utestSettings)
    .settings(
      moduleName := "ext-nyaya",
      libraryDependencies ++= Seq(
        "com.github.japgolly.nyaya" %%% "nyaya-gen" % Ver.Nyaya,
        "com.github.japgolly.nyaya" %%% "nyaya-test" % Ver.Nyaya))

  lazy val extScalaJsReact = project
    .in(file("ext-scalajs-react"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, publicationSettings.js, utestSettings.js)
    .dependsOn(domZipperJS)
    .settings(
      moduleName := "ext-scalajs-react",
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "core" % Ver.ScalaJsReact,
        "com.github.japgolly.scalajs-react" %%% "test" % Ver.ScalaJsReact),
      jsEnv := new JSDOMNodeJSEnv)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val examples =
    Project("examples", file(".examples"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(exampleSelenium, exampleReactJS)

  lazy val exampleSelenium = project
    .in(file("example-selenium"))
    .configure(commonSettings.jvm, preventPublication, utestSettings.jvm)
    .dependsOn(coreJVM, domZipperSelenium)
    .settings(
      moduleName := "example-selenium",
      libraryDependencies ++= Seq(
        "org.seleniumhq.selenium" % "selenium-chrome-driver"  % Ver.Selenium % Test,
        "org.seleniumhq.selenium" % "selenium-firefox-driver" % Ver.Selenium % Test),
      fork in Test := true,
      javaOptions in Test += ("-DCI=" + System.getProperty("CI", "")))

  lazy val exampleReactJS = project
    .in(file("example-react"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, preventPublication, utestSettings.js)
    .dependsOn(coreJS, domZipperSizzle, extScalaJsReact)
    .settings(
      moduleName := "example-react",
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "ext-monocle"   % Ver.ScalaJsReact,
        "com.github.julien-truffaut"        %%% "monocle-core"  % Ver.Monocle,
        "com.github.julien-truffaut"        %%% "monocle-macro" % Ver.Monocle),
      addCompilerPlugin(macroParadisePlugin), // For Monocle macros
      jsDependencies ++= Seq(
        "org.webjars.npm" % "react" % Ver.ReactJs
          /        "umd/react.development.js"
          minified "umd/react.production.min.js"
          commonJSName "React",
        "org.webjars.npm" % "react-dom" % Ver.ReactJs
          /         "umd/react-dom.development.js"
          minified  "umd/react-dom.production.min.js"
          dependsOn "umd/react.development.js"
          commonJSName "ReactDOM",
        "org.webjars.npm" % "react-dom" % Ver.ReactJs
          /         "umd/react-dom-server.browser.development.js"
          minified  "umd/react-dom-server.browser.production.min.js"
          dependsOn "umd/react-dom.development.js"
          commonJSName "ReactDOMServer",
        "org.webjars.npm" % "react-dom" % Ver.ReactJs % Test
          /         "umd/react-dom-test-utils.development.js"
          minified  "umd/react-dom-test-utils.production.min.js"
          dependsOn "umd/react-dom.development.js"
          commonJSName "ReactTestUtils"))
}
