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

object TestState {

  private val ghProject = "test-state"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    final val Acyclic         = "0.1.8"
    final val Cats            = "1.6.1"
    final val Jsoup           = "1.11.3"
    final val KindProjector   = "0.9.10"
    final val MacroParadise   = "2.1.1"
    final val Microlibs       = "1.18"
    final val MTest           = "0.6.6"
    final val Nyaya           = "0.8.1"
    final val Scala211        = "2.11.12"
    final val Scala212        = "2.12.7"
    final val ScalaJsDom      = "0.9.7"
    final val ScalaJsReact    = "1.3.1"
    final val ScalaJsJavaTime = "0.2.5"
    final val Scalaz          = "7.2.27"
    final val Selenium        = "3.141.59"
    final val Sizzle          = "2.3.0"
    final val UnivEq          = "1.0.6"

    // Used in examples only
    final val Monocle       = "1.5.0"
    final val ReactJs       = "16.5.1"
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
      organization                  := "com.github.japgolly.test-state",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala212,
      crossScalaVersions            := Seq(Ver.Scala211, Ver.Scala212),
      scalacOptions                ++= scalacFlags,
      scalacOptions in Compile     ++= byScalaVersion { case (2, 12) => Seq("-opt:l:method") }.value,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code"),
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage              := Watched.clearWhenTriggered,
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin("org.spire-math" %% "kind-projector" % Ver.KindProjector))
    .configure(acyclicSettings))

  def byScalaVersion[A](f: PartialFunction[(Long, Long), Seq[A]]): Def.Initialize[Seq[A]] =
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

  def testSettings = ConfigureBoth(
    _.settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi"                   %%% "utest"     % Ver.MTest     % Test,
        "com.github.japgolly.microlibs" %%% "test-util" % Ver.Microlibs % Test),
      testFrameworks += new TestFramework("utest.runner.Framework")))
    .jsConfigure(
      _.settings(jsEnv in Test := new JSDOMNodeJSEnv))

  def testSettingsCI =
    testSettings.jvmConfigure(_.settings(
      fork in Test := true,
      javaOptions in Test += ("-DCI=" + System.getProperty("CI", ""))))

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
        domZipperJVM, domZipperJsoup, domZipperSelenium,
        extScalazJVM, extCatsJVM, extNyayaJVM, extSelenium,
        utilJVM, utilSelenium)

  lazy val rootJS =
    Project("JS", file(".rootJS"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        coreJS, coreMacrosJS,
        domZipperJS, domZipperSizzle,
        extScalazJS, extCatsJS, extNyayaJS, extScalaJsReact,
        utilJS)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val coreMacrosJVM = coreMacros.jvm
  lazy val coreMacrosJS  = coreMacros.js
  lazy val coreMacros = crossProject(JSPlatform, JVMPlatform)
    .in(file("core-macros"))
    .configureCross(commonSettings, publicationSettings, testSettings)
    .bothConfigure(definesMacros)
    .settings(moduleName := "core-macros")

  lazy val coreJVM = core.jvm
  lazy val coreJS  = core.js
  lazy val core = crossProject(JSPlatform, JVMPlatform)
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(coreMacros, util)
    .configureCross(testSettings)
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
  lazy val domZipper = crossProject(JSPlatform, JVMPlatform)
    .in(file("dom-zipper"))
    .configureCross(commonSettings, publicationSettings, testSettings)
    .dependsOn(util)
    .settings(moduleName := "dom-zipper")
    .jsSettings(
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % Ver.ScalaJsDom,
      jsEnv               := new JSDOMNodeJSEnv)

  lazy val domZipperJsoup = project
    .in(file("dom-zipper-jsoup"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettingsCI.jvm)
    .dependsOn(domZipperJVM)
    .settings(
      moduleName := "dom-zipper-jsoup",
      libraryDependencies += "org.jsoup" % "jsoup" % Ver.Jsoup)

  lazy val domZipperSelenium = project
    .in(file("dom-zipper-selenium"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettingsCI.jvm)
    .dependsOn(domZipperJVM, utilSelenium, domZipperJsoup)
    .settings(
      moduleName := "dom-zipper-selenium",
      libraryDependencies ++= Seq(
        "org.seleniumhq.selenium" % "selenium-chrome-driver"  % Ver.Selenium % Test,
        "org.seleniumhq.selenium" % "selenium-firefox-driver" % Ver.Selenium % Test),
      javaOptions in Test += ("-Dsbt.baseDirectory=" + baseDirectory.value.getAbsolutePath))

  lazy val domZipperSizzle = project
    .in(file("dom-zipper-sizzle"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, publicationSettings.js, testSettings.js)
    .dependsOn(domZipperJS)
    .settings(
      moduleName     := "dom-zipper-sizzle",
      scalacOptions  -= "-Ywarn-dead-code",
      jsDependencies += "org.webjars.bower" % "sizzle" % Ver.Sizzle / "sizzle.min.js" commonJSName "Sizzle",
      jsEnv          := new JSDOMNodeJSEnv)

  lazy val extScalazJVM = extScalaz.jvm
  lazy val extScalazJS  = extScalaz.js
  lazy val extScalaz = crossProject(JSPlatform, JVMPlatform)
    .in(file("ext-scalaz"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(core)
    .configureCross(testSettings)
    .settings(
      moduleName          := "ext-scalaz",
      libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val extCatsJVM = extCats.jvm
  lazy val extCatsJS  = extCats.js
  lazy val extCats = crossProject(JSPlatform, JVMPlatform)
    .in(file("ext-cats"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(core)
    .configureCross(testSettings)
    .settings(
      moduleName          := "ext-cats",
      libraryDependencies += "org.typelevel" %%% "cats-core" % Ver.Cats)

  lazy val extNyayaJVM = extNyaya.jvm
  lazy val extNyayaJS  = extNyaya.js
  lazy val extNyaya = crossProject(JSPlatform, JVMPlatform)
    .in(file("ext-nyaya"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(core, extScalaz)
    .configureCross(testSettings)
    .settings(
      moduleName := "ext-nyaya",
      libraryDependencies ++= Seq(
        "com.github.japgolly.nyaya" %%% "nyaya-gen" % Ver.Nyaya,
        "com.github.japgolly.nyaya" %%% "nyaya-test" % Ver.Nyaya))

  lazy val extScalaJsReact = project
    .in(file("ext-scalajs-react"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, publicationSettings.js, testSettings.js)
    .dependsOn(domZipperJS)
    .settings(
      moduleName := "ext-scalajs-react",
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "core" % Ver.ScalaJsReact,
        "com.github.japgolly.scalajs-react" %%% "test" % Ver.ScalaJsReact),
      jsEnv := new JSDOMNodeJSEnv)

  lazy val extSelenium = project
    .in(file("ext-selenium"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettingsCI.jvm)
    .dependsOn(coreJVM, utilSelenium)
    .settings(
      moduleName := "ext-selenium",
      libraryDependencies ++= Seq(
        "org.seleniumhq.selenium" % "selenium-chrome-driver"  % Ver.Selenium,
        "org.seleniumhq.selenium" % "selenium-firefox-driver" % Ver.Selenium))

  lazy val utilJVM = util.jvm
  lazy val utilJS  = util.js
  lazy val util = crossProject(JSPlatform, JVMPlatform)
    .configureCross(commonSettings, publicationSettings, testSettingsCI)

  lazy val utilSelenium = project
    .in(file("util-selenium"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettingsCI.jvm)
    .dependsOn(utilJVM)
    .settings(
      moduleName := "util-selenium",
      libraryDependencies ++= Seq(
        "org.seleniumhq.selenium" % "selenium-api" % Ver.Selenium,
        "org.seleniumhq.selenium" % "selenium-remote-driver" % Ver.Selenium))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val examples =
    Project("examples", file(".examples"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(exampleSelenium, exampleReactJS)

  lazy val exampleSelenium = project
    .in(file("example-selenium"))
    .configure(commonSettings.jvm, preventPublication, testSettingsCI.jvm)
    .dependsOn(coreJVM, domZipperSelenium, extSelenium)
    .settings(moduleName := "example-selenium")

  lazy val exampleReactJS = project
    .in(file("example-react"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, preventPublication, testSettings.js)
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
