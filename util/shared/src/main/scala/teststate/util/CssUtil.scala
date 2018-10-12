package teststate.util

object CssUtil {

  def disableCssAnimation(disableTransitions: Boolean = true,
                          disableTransforms: Boolean = true,
                          disableAnimation: Boolean = true): String = {
    val sb = new StringBuilder
    sb.append("* {\n")
    if (disableTransitions) {
      sb.append("/*CSS transitions*/\n")
      sb.append(   "-moz-transition-property: none !important;\n")
      sb.append(    "-ms-transition-property: none !important;\n")
      sb.append(     "-o-transition-property: none !important;\n")
      sb.append("-webkit-transition-property: none !important;\n")
      sb.append(        "transition-property: none !important;\n")
    }
    if (disableTransforms) {
      sb.append("/*CSS transforms*/\n")
      sb.append(   "-moz-transform: none !important;\n")
      sb.append(    "-ms-transform: none !important;\n")
      sb.append(     "-o-transform: none !important;\n")
      sb.append("-webkit-transform: none !important;\n")
      sb.append(        "transform: none !important;\n")
    }
    if (disableAnimation) {
      sb.append("/*CSS animations*/\n")
      sb.append(   "-moz-animation: none !important;\n")
      sb.append(    "-ms-animation: none !important;\n")
      sb.append(     "-o-animation: none !important;\n")
      sb.append("-webkit-animation: none !important;\n")
      sb.append(        "animation: none !important;\n")
    }
    sb.append("}")
    sb.toString()
  }

}
