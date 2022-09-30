name := "TestState"

ThisBuild / homepage      := Some(url("https://github.com/japgolly/test-state"))
ThisBuild / licenses      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0"))
ThisBuild / organization  := "com.github.japgolly.test-state"
ThisBuild / shellPrompt   := ((s: State) => Project.extract(s).currentRef.project + "> ")
ThisBuild / startYear     := Some(2016)
ThisBuild / versionScheme := Some("early-semver")

sonatypeProfileName := "com.github.japgolly"

val root              = TestState.root
val rootJVM           = TestState.rootJVM
val rootJS            = TestState.rootJS

val utilJVM           = TestState.utilJVM
val utilJS            = TestState.utilJS
val utilSelenium      = TestState.utilSelenium

val coreJVM           = TestState.coreJVM
val coreJS            = TestState.coreJS
val domZipperJVM      = TestState.domZipperJVM
val domZipperJS       = TestState.domZipperJS
val domZipperJsoup    = TestState.domZipperJsoup
val domZipperSelenium = TestState.domZipperSelenium
val domZipperSizzle   = TestState.domZipperSizzle
val extCatsJVM        = TestState.extCatsJVM
val extCatsJS         = TestState.extCatsJS
val extNyayaJVM       = TestState.extNyayaJVM
val extNyayaJS        = TestState.extNyayaJS
val extScalaJsReact   = TestState.extScalaJsReact
val extSelenium       = TestState.extSelenium

val examples          = TestState.examples
val exampleSelenium   = TestState.exampleSelenium
val exampleReactJS    = TestState.exampleReactJS
