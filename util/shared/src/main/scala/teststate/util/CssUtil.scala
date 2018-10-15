package teststate.util

object CssUtil {

  val DefaultPrefixes = "-moz-" :: "-ms-" :: "-o-" :: "-webkit-" :: Nil

  def disableCssAnimation(disableAnimation  : Boolean = true,
                          disableTransitions: Boolean = true,
                          disableTransforms : Boolean = true): String = {
    val sb = new StringBuilder
    def set(prop: String, value: String): Unit = {
      sb.append(prop)
      sb.append(':')
      sb.append(value)
      sb.append("!important;\n")
      ()
    }

    def setWithPrefixes(prop: String, value: String): Unit = {
      for (pre <- DefaultPrefixes) {
        sb.append(pre)
        set(prop, value)
      }
      set(prop, value)
    }

    sb.append("* {\n")
    if (disableTransitions) {
      sb.append("/*CSS transitions*/\n")
      setWithPrefixes("transition-property", "none")
      setWithPrefixes("transition-delay", "0s")
      setWithPrefixes("transition-duration", "0s")
    }
    if (disableTransforms) {
      sb.append("/*CSS transforms*/\n")
      setWithPrefixes("transform", "none")
    }
    if (disableAnimation) {
      sb.append("/*CSS animations*/\n")
      setWithPrefixes("animation", "none")
      setWithPrefixes("animation-delay", "0s")
      setWithPrefixes("animation-duration", "0s")
      setWithPrefixes("animation-iteration-count", "0")
    }
    sb.append("}")
    sb.toString()
  }

}
