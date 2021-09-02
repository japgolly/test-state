import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._

object Dependencies {

  object Ver {

    // Exported
    val cats            = "2.6.1"
    val jsoup           = "1.14.2"
    val microlibs       = "3.0.1"
    val nyaya           = "0.11.0"
    val scala2          = "2.13.6"
    val scala3          = "3.0.2"
    val scalaJsDom      = "1.2.0"
    val scalaJsReact    = "2.0.0-RC2"
    val selenium        = "3.141.59"
    val sizzle          = "2.3.0"
    val univEq          = "1.6.0"

    // Internal
    val kindProjector   = "0.13.1"
    val monocle         = "3.0.0"
    val reactJs         = "17.0.2"
    val scalaJsJavaTime = "1.0.0"
    val utest           = "0.7.10"
  }

  object Dep {
    val cats                 = Def.setting("org.typelevel"                     %%% "cats-core"               % Ver.cats)
    val jsoup                = Def.setting("org.jsoup"                           % "jsoup"                   % Ver.jsoup)
    val microlibsNameFn      = Def.setting("com.github.japgolly.microlibs"     %%% "name-fn"                 % Ver.microlibs)
    val microlibsTestUtil    = Def.setting("com.github.japgolly.microlibs"     %%% "test-util"               % Ver.microlibs)
    val monocleCore          = Def.setting("dev.optics"                        %%% "monocle-core"            % Ver.monocle)
    val monocleMacro         = Def.setting("dev.optics"                        %%% "monocle-macro"           % Ver.monocle)
    val nyayaGen             = Def.setting("com.github.japgolly.nyaya"         %%% "nyaya-gen"               % Ver.nyaya)
    val nyayaProp            = Def.setting("com.github.japgolly.nyaya"         %%% "nyaya-prop"              % Ver.nyaya)
    val nyayaTest            = Def.setting("com.github.japgolly.nyaya"         %%% "nyaya-test"              % Ver.nyaya)
    val scalaJsDom           = Def.setting("org.scala-js"                      %%% "scalajs-dom"             % Ver.scalaJsDom cross CrossVersion.for3Use2_13)
    val scalaJsJavaTime      = Def.setting("org.scala-js"                      %%% "scalajs-java-time"       % Ver.scalaJsJavaTime cross CrossVersion.for3Use2_13)
    val scalaJsReactCore     = Def.setting("com.github.japgolly.scalajs-react" %%% "core"                    % Ver.scalaJsReact)
    val scalaJsReactMonocle  = Def.setting("com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3"      % Ver.scalaJsReact)
    val scalaJsReactTest     = Def.setting("com.github.japgolly.scalajs-react" %%% "test"                    % Ver.scalaJsReact)
    val seleniumApi          = Def.setting("org.seleniumhq.selenium"             % "selenium-api"            % Ver.selenium)
    val seleniumChrome       = Def.setting("org.seleniumhq.selenium"             % "selenium-chrome-driver"  % Ver.selenium)
    val seleniumFirefox      = Def.setting("org.seleniumhq.selenium"             % "selenium-firefox-driver" % Ver.selenium)
    val seleniumRemoteDriver = Def.setting("org.seleniumhq.selenium"             % "selenium-remote-driver"  % Ver.selenium)
    val sizzle               = Def.setting("org.webjars.bower"                   % "sizzle"                  % Ver.sizzle / "sizzle.min.js" commonJSName "Sizzle")
    val univEq               = Def.setting("com.github.japgolly.univeq"        %%% "univeq"                  % Ver.univEq)
    val utest                = Def.setting("com.lihaoyi"                       %%% "utest"                   % Ver.utest)

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
