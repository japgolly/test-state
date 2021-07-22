import sbt._
import sbt.Keys._
import com.jsuereth.sbtpgp.PgpKeys
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, _}
import sbtrelease.ReleasePlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import Dependencies._
import Lib._

object TestState {

  private val ghProject = "test-state"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-opt:l:inline",
    "-opt-inline-from:japgolly.univeq.**",
    "-opt-inline-from:teststate.**",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ywarn-value-discard")

  val commonSettings = ConfigureBoth(
    _.settings(
      organization                  := "com.github.japgolly.test-state",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.scala2,
      crossScalaVersions            := Seq(Ver.scala2),
      scalacOptions                ++= scalacFlags,
      Test / scalacOptions         --= Seq("-Ywarn-dead-code"),
      ThisBuild / shellPrompt       := ((s: State) => Project.extract(s).currentRef.project + "> "),
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(ThisBuild / version).value}",
      releaseVcsSign                := true,
      libraryDependencies          ++= Seq(Dep.kindProjector).filterNot(_ => scalaVersion.value.startsWith("3")),
  ))

  def byScalaVersion[A](f: PartialFunction[(Long, Long), Seq[A]]): Def.Initialize[Seq[A]] =
    Def.setting(CrossVersion.partialVersion(scalaVersion.value).flatMap(f.lift).getOrElse(Nil))

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        // "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided))

  def addMacroParadisePlugin = Def.settings(
    Seq(
      scalacOptions ++= byScalaVersion {
        case (2, 13) => Seq("-Ymacro-annotations")
      }.value
    ))

  def testSettings = ConfigureBoth(
    _.settings(
      libraryDependencies ++= Seq(
        Dep.utest            .value % Test,
        Dep.microlibsTestUtil.value % Test,
      ),
      testFrameworks += new TestFramework("utest.runner.Framework")))
    .jsConfigure(
      _.settings(Test / jsEnv := new JSDOMNodeJSEnv))

  def testSettingsCI =
    testSettings.jvmConfigure(_.settings(
      Test / fork := true,
      Test / javaOptions += ("-DCI=" + System.getProperty("CI", ""))))

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
        extCatsJVM, extNyayaJVM, extSelenium,
        utilJVM, utilSelenium)

  lazy val rootJS =
    Project("JS", file(".rootJS"))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        coreJS, coreMacrosJS,
        domZipperJS, domZipperSizzle,
        extCatsJS, extNyayaJS, extScalaJsReact,
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
        Dep.univEq   .value,
        Dep.nyayaGen .value % Test,
        Dep.nyayaProp.value % Test,
        Dep.nyayaTest.value % Test,
    ))
    .jsSettings(
      libraryDependencies += Dep.scalaJsJavaTime.value % Provided)

  lazy val domZipperJVM = domZipper.jvm
  lazy val domZipperJS  = domZipper.js
  lazy val domZipper = crossProject(JSPlatform, JVMPlatform)
    .in(file("dom-zipper"))
    .configureCross(commonSettings, publicationSettings, testSettings)
    .dependsOn(util)
    .settings(moduleName := "dom-zipper")
    .jsSettings(
      libraryDependencies += Dep.scalaJsDom.value,
      jsEnv               := new JSDOMNodeJSEnv)

  lazy val domZipperJsoup = project
    .in(file("dom-zipper-jsoup"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettingsCI.jvm)
    .dependsOn(domZipperJVM)
    .settings(
      moduleName := "dom-zipper-jsoup",
      libraryDependencies += Dep.jsoup.value)

  lazy val domZipperSelenium = project
    .in(file("dom-zipper-selenium"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettingsCI.jvm)
    .dependsOn(domZipperJVM, utilSelenium, domZipperJsoup)
    .settings(
      moduleName := "dom-zipper-selenium",
      libraryDependencies ++= Seq(
        Dep.seleniumChrome .value % Test,
        Dep.seleniumFirefox.value % Test,
      ),
      Test / javaOptions += ("-Dsbt.baseDirectory=" + baseDirectory.value.getAbsolutePath))

  lazy val domZipperSizzle = project
    .in(file("dom-zipper-sizzle"))
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(JSDependenciesPlugin)
    .configure(commonSettings.js, publicationSettings.js, testSettings.js)
    .dependsOn(domZipperJS)
    .settings(
      moduleName     := "dom-zipper-sizzle",
      scalacOptions  -= "-Ywarn-dead-code",
      jsDependencies += Dep.sizzle.value,
      jsEnv          := new JSDOMNodeJSEnv)

  lazy val extCatsJVM = extCats.jvm
  lazy val extCatsJS  = extCats.js
  lazy val extCats = crossProject(JSPlatform, JVMPlatform)
    .in(file("ext-cats"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(core)
    .configureCross(testSettings)
    .settings(
      moduleName          := "ext-cats",
      libraryDependencies += Dep.cats.value)

  lazy val extNyayaJVM = extNyaya.jvm
  lazy val extNyayaJS  = extNyaya.js
  lazy val extNyaya = crossProject(JSPlatform, JVMPlatform)
    .in(file("ext-nyaya"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(core, extCats)
    .configureCross(testSettings)
    .settings(
      moduleName := "ext-nyaya",
      libraryDependencies ++= Seq(
        Dep.nyayaGen.value,
        Dep.nyayaTest.value,
    ))

  lazy val extScalaJsReact = project
    .in(file("ext-scalajs-react"))
    .enablePlugins(ScalaJSPlugin)
    .configure(commonSettings.js, publicationSettings.js, testSettings.js)
    .dependsOn(coreJS, domZipperJS)
    .settings(
      moduleName := "ext-scalajs-react",
      libraryDependencies ++= Seq(
        Dep.scalaJsReactCore.value,
        Dep.scalaJsReactTest.value,
        Dep.scalaJsJavaTime.value % Test,
      ),
      jsEnv := new JSDOMNodeJSEnv)

  lazy val extSelenium = project
    .in(file("ext-selenium"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettingsCI.jvm)
    .dependsOn(coreJVM, utilSelenium)
    .settings(
      moduleName := "ext-selenium",
      libraryDependencies ++= Seq(
        Dep.seleniumChrome.value,
        Dep.seleniumFirefox.value,
    ))

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
        Dep.seleniumApi         .value,
        Dep.seleniumRemoteDriver.value,
    ))

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
    .enablePlugins(JSDependenciesPlugin)
    .configure(commonSettings.js, preventPublication, testSettings.js, addReactJsDependencies(Compile))
    .dependsOn(coreJS, domZipperSizzle, extScalaJsReact)
    .settings(
      moduleName := "example-react",
      libraryDependencies ++= Seq(
        Dep.scalaJsReactMonocle.value,
        Dep.monocleCore        .value,
        Dep.monocleMacro       .value,
        Dep.scalaJsJavaTime    .value,
      ),
      addMacroParadisePlugin, // For Monocle macros
    )
}
