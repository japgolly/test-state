package teststate.domzipper

import Exports._

final class DomZipperJsCompilationTest($: DomZipperJs) {

  // What I don't want to see is:
  // private value $ escapes its defining scope as part of type DomZipperJsCompilationTest.this.$.Collection[Vector]
  def asd = $.collect0n("haha")
}
