package teststate.selenium

import org.openqa.selenium.WebDriver

object Exports extends Exports

trait Exports extends SeleniumExt {

  final type GrowthStrategy                = teststate.selenium.GrowthStrategy
  final val  GrowthStrategy                = teststate.selenium.GrowthStrategy

  final type MultiBrowser[+D <: WebDriver] = teststate.selenium.MultiBrowser[D]
  final val  MultiBrowser                  = teststate.selenium.MultiBrowser

  final type MultiTab[+D <: WebDriver]     = teststate.selenium.MultiTab[D]
  final val  MultiTab                      = teststate.selenium.MultiTab

  final type Tab[+D <: WebDriver]          = teststate.selenium.Tab[D]

  final type TabAlreadyClosed              = teststate.selenium.TabAlreadyClosed

  final type TabSupport[-D <: WebDriver]   = teststate.selenium.TabSupport[D]
  final val  TabSupport                    = teststate.selenium.TabSupport

}

