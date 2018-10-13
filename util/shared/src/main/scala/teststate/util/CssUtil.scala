package teststate.util

object CssUtil {

  val DefaultPrefixes = "-moz-" :: "-ms-" :: "-o-" :: "-webkit-" :: Nil

  def disableCssAnimation(disableTransitions: Boolean = true,
                          disableTransforms : Boolean = true,
                          disableAnimation  : Boolean = true): String = {
    val sb = new StringBuilder
    def disable(prop: String): Unit = {
      sb.append(prop)
      sb.append(": none !important;\n")
      ()
    }

    def disableWithPrefixes(prop: String): Unit = {
      for (pre <- DefaultPrefixes) {
        sb.append(pre)
        disable(prop)
      }
      disable(prop)
    }

    sb.append("* {\n")
    if (disableTransitions) {
      sb.append("/*CSS transitions*/\n")
      disableWithPrefixes("transition-property")
    }
    if (disableTransforms) {
      sb.append("/*CSS transforms*/\n")
      disableWithPrefixes("transform")
    }
    if (disableAnimation) {
      sb.append("/*CSS animations*/\n")
      disableWithPrefixes("animation")
    }
    sb.append("}")
    sb.toString()
  }

}
