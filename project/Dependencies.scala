import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._

object Dependencies {

  object Ver {

    // Exported
    val cats            = "2.13.0"
    val catsEffect      = "3.7.1"
    val jsoup           = "1.24.1"
    val microlibs       = "4.3.0"
    val nyaya           = "1.2.0"
    val scala2          = "2.13.18"
    val scala3          = "3.3.9"
    val scalaJsDom      = "2.8.1"
    val scalaJsReact    = "2.1.4"
    val selenium        = "4.54.0"
    val sizzle          = "2.3.0"
    val univEq          = "2.1.0"
    val zio             = "2.1.27"

    // Internal
    val http4s          = "0.23.39"
    val kindProjector   = "0.13.4"
    val monocle         = "3.3.0"
    val reactJs         = "17.0.2"
    val scalaJsJavaTime = "2.4.0"
    val sttpClient      = "4.0.27"
    val tapir           = "1.13.32"
    val utest           = "0.8.4"
  }

  object Dep {
    val cats                 = Def.setting("org.typelevel"                     %%% "cats-core"               % Ver.cats)
    val catsEffect           = Def.setting("org.typelevel"                     %%% "cats-effect"             % Ver.catsEffect)
    val http4sServer         = Def.setting("org.http4s"                         %% "http4s-ember-server"     % Ver.http4s)
    val jsoup                = Def.setting("org.jsoup"                           % "jsoup"                   % Ver.jsoup)
    val microlibsNameFn      = Def.setting("com.github.japgolly.microlibs"     %%% "name-fn"                 % Ver.microlibs)
    val microlibsTestUtil    = Def.setting("com.github.japgolly.microlibs"     %%% "test-util"               % Ver.microlibs)
    val monocleCore          = Def.setting("dev.optics"                        %%% "monocle-core"            % Ver.monocle)
    val monocleMacro         = Def.setting("dev.optics"                        %%% "monocle-macro"           % Ver.monocle)
    val nyayaGen             = Def.setting("com.github.japgolly.nyaya"         %%% "nyaya-gen"               % Ver.nyaya)
    val nyayaProp            = Def.setting("com.github.japgolly.nyaya"         %%% "nyaya-prop"              % Ver.nyaya)
    val nyayaTest            = Def.setting("com.github.japgolly.nyaya"         %%% "nyaya-test"              % Ver.nyaya)
    val scalaJsDom           = Def.setting("org.scala-js"                      %%% "scalajs-dom"             % Ver.scalaJsDom)
    val scalaJsJavaTime      = Def.setting("io.github.cquiroz"                 %%% "scala-java-time"         % Ver.scalaJsJavaTime)
    val scalaJsReactCore     = Def.setting("com.github.japgolly.scalajs-react" %%% "core"                    % Ver.scalaJsReact)
    val scalaJsReactMonocle  = Def.setting("com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3"      % Ver.scalaJsReact)
    val scalaJsReactTest     = Def.setting("com.github.japgolly.scalajs-react" %%% "test"                    % Ver.scalaJsReact)
    val seleniumApi          = Def.setting("org.seleniumhq.selenium"             % "selenium-api"            % Ver.selenium)
    val seleniumChrome       = Def.setting("org.seleniumhq.selenium"             % "selenium-chrome-driver"  % Ver.selenium)
    val seleniumFirefox      = Def.setting("org.seleniumhq.selenium"             % "selenium-firefox-driver" % Ver.selenium)
    val seleniumRemoteDriver = Def.setting("org.seleniumhq.selenium"             % "selenium-remote-driver"  % Ver.selenium)
    val sizzle               = Def.setting("org.webjars.bower"                   % "sizzle"                  % Ver.sizzle / "sizzle.min.js" commonJSName "Sizzle")
    val sttpClientCats       = Def.setting("com.softwaremill.sttp.client4"      %% "cats"                    % Ver.sttpClient)
    val sttpClientCirce      = Def.setting("com.softwaremill.sttp.client4"      %% "circe"                   % Ver.sttpClient)
    val tapirCore            = Def.setting("com.softwaremill.sttp.tapir"        %% "tapir-core"              % Ver.tapir)
    val tapirHttp4sServer    = Def.setting("com.softwaremill.sttp.tapir"        %% "tapir-http4s-server"     % Ver.tapir)
    val tapirJsonCirce       = Def.setting("com.softwaremill.sttp.tapir"        %% "tapir-json-circe"        % Ver.tapir)
    val tapirSttpClient      = Def.setting("com.softwaremill.sttp.tapir"        %% "tapir-sttp-client4"      % Ver.tapir)
    val univEq               = Def.setting("com.github.japgolly.univeq"        %%% "univeq"                  % Ver.univEq)
    val utest                = Def.setting("com.lihaoyi"                       %%% "utest"                   % Ver.utest)
    val zio                  = Def.setting("dev.zio"                           %%% "zio"                     % Ver.zio)

    // Compiler plugins
    val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % Ver.kindProjector cross CrossVersion.full)
  }

  def addReactJsDependencies(scope: Configuration): Project => Project =
    _.enablePlugins(JSDependenciesPlugin)
      .settings(
        jsDependencies ++= Seq(

          "org.webjars.npm" % "react" % Ver.reactJs % scope
            /        "umd/react.development.js"
            minified "umd/react.production.min.js"
            commonJSName "React",

          "org.webjars.npm" % "react-dom" % Ver.reactJs % scope
            /         "umd/react-dom.development.js"
            minified  "umd/react-dom.production.min.js"
            dependsOn "umd/react.development.js"
            commonJSName "ReactDOM",

          "org.webjars.npm" % "react-dom" % Ver.reactJs % scope
            /         "umd/react-dom-test-utils.development.js"
            minified  "umd/react-dom-test-utils.production.min.js"
            dependsOn "umd/react-dom.development.js"
            commonJSName "ReactTestUtils",

          "org.webjars.npm" % "react-dom" % Ver.reactJs % scope
            /         "umd/react-dom-server.browser.development.js"
            minified  "umd/react-dom-server.browser.production.min.js"
            dependsOn "umd/react-dom.development.js"
            commonJSName "ReactDOMServer"),

        packageJSDependencies / skip := false)

}
