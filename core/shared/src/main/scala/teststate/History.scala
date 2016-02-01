package teststate

import scala.annotation.elidable
import History.{Options, Step, Steps}

final class History[+E](val steps: Steps[E], val result: Result[E]) {
  override def toString = s"History($steps, $result)"

  def failure = result.failure
  def failed = failure.isDefined
  def isEmpty = steps.isEmpty
  @inline def nonEmpty = !isEmpty

  def :+[e >: E](s: Step[e]) = this ++ vector1(s)
  def ++[e >: E](s: Steps[e]): History[e] = {
    val result2 = s.foldLeft[Result[e]](result)(_ + _.result)
    new History[e](steps ++ s, result2)
  }
  def ++[e >: E](s: History[e]): History[e] =
    new History[e](steps ++ s.steps, result + s.result)

  def resultStep: Option[Step[E]] =
    steps.find(_.result eq result)

  def rootFailurePath: Vector[Step[E]] =
    if (failed)
      resultStep match {
        case Some(s) => s +: s.children.rootFailurePath
        case None => Vector.empty
      }
    else
      Vector.empty

  //def +:[e >: E](s: Step[e]) = new History[e](s +: steps, result)
  //def ++[e >: E](s: Steps[e]) = new History[e](steps ++ s, result)

  def unlessFailed[e >: E](f: History[E] => History[e]): History[e] =
    if (failed)
      this
    else
      f(this)

  def format[e >: E](options: Options)(implicit showError: ShowError[e]): String = {
    val sb = new StringBuilder

    def appendIndent(indent: Int): Unit = {
      var i = indent
      while (i > 0) {
        sb append options.indent
        i -= 1
      }
    }

    def appendResultFlag(r: Result[E]): Unit = {
      sb append (r match {
        case Result.Pass    => options.onPass
        case Result.Skip    => options.onSkip
        case Result.Fail(_) => options.onFail
      })
      ()
    }

    def showHistory(h: History[E], indent: Int): Unit =
      showSteps(h.steps, indent)

    def showSteps(steps: Steps[E], indent: Int): Unit =
      steps foreach (showStep(_, indent))

    def showStep(step: Step[E], indent: Int): Unit = {
      val showChildren = step.children.steps.nonEmpty && options.showChildren(step.children)
      val error = if (showChildren) None else step.result.failure

      appendIndent(indent)
      appendResultFlag(step.result)
      sb append ' '
      sb append step.name.value
      for (err <- error) {
        val e = showError.show(err)
        if (e.nonEmpty) {
          sb append " -- "
          sb append e
        }
      }
      sb append options.eol

      if (showChildren)
        showHistory(step.children, indent + 1)
    }

    showHistory(this, 0)

    sb.result()
  }

  def failureReason[e >: E](implicit showError: ShowError[e]): Option[String] =
    failure.map(e =>
      (rootFailurePath.lastOption, Option(showError show e).filter(_.nonEmpty)) match {
        case (Some(f), Some(e)) => s"${f.name.value} -- $e"
        case (Some(f), None   ) => f.name.value
        case (None   , Some(e)) => e
        case (None   , None   ) => "Failed without an error message."
      }
    )

  @elidable(elidable.ASSERTION)
  def assert[e >: E](options: Options)(implicit showError: ShowError[e]): Unit =
    for (err <- failureReason[e]) {
      println(format[e](options))
      throw new AssertionError(err)
    }
}


object History {
  val empty = History(Vector.empty, Result.empty)

  type Steps[+Err] = Vector[Step[Err]]

  case class Step[+E](name: Name, result: Result[E], children: History[E] = empty) {
    def failure = result.failure
    def failed = failure.isDefined
  }

  def parent[E](name: Name, children: History[E]): Step[E] =
    Step(name, children.result, children)

  def maybeParent[E](name: Name, children: History[E]): History[E] =
    if (children.isEmpty)
      History.empty
    else
      History(parent(name, children))

  def determineResult[E](steps: Steps[E]): Result[E] =
    steps.foldLeft(Result.empty[E])(_ + _.result)

  def apply[E](step: Step[E]): History[E] =
    new History(vector1(step), step.result)

  def apply[E](steps: Steps[E]): History[E] =
    new History(steps, determineResult(steps))

  def apply[E](steps: Steps[E], result: Result[E]): History[E] =
    new History(steps, result)

  def newBuilder[E] = new Builder[E]
  final class Builder[E] {
    private val b = Vector.newBuilder[Step[E]]
    private var r = Result.empty[E]

    def +=(s: Step[E]): Unit = {
      b += s
      r += s.result
    }

    def ++=(h: History[E]): Unit =
      if (h.nonEmpty) {
        b ++= h.steps
        r += h.result
      }

    def addEach[A](as: Vector[A])(name: A => Name, test: A => Option[E])(implicit recover: Recover[E]): Unit =
      for (a <- as) {
        val n = Recover name name(a)
        val r = recover.recover(Result passOrFail test(a), Result.Fail(_))
        this += Step(n, r)
      }

    def result(): Result[E] =
      r

    def steps(): Steps[E] =
      b.result()

    def history(): History[E] =
      History(steps(), result())

    def group(name: Name): History[E] =
      History.maybeParent(name, history())
  }

  case class Options(indent: String,
                     onPass: String,
                     onSkip: String,
                     onFail: String,
                     eol: String,
                     showChildren: History[Any] => Boolean) {

    def alwaysShowChildren =
      copy(showChildren = _ => true)

    def onlyShowFailedChildren =
      copy(showChildren = _.failure.isDefined)
  }

  object Options {

    val uncolored = Options(
      indent       = "  ",
      onPass       = "✓", // ✓ ✔
      onSkip       = "-", // ⇣ ↶ ↷
      onFail       = "✘", // ✗ ✘
      eol          = "\n",
      showChildren = _.failure.isDefined)

    import scala.Console._

    val colored = Options(
      indent       = "  ",
      onPass       = BOLD + GREEN + uncolored.onPass + RESET + WHITE,
      onSkip       = BOLD + YELLOW + uncolored.onSkip + BLACK,
      onFail       = RED + uncolored.onFail + BOLD,
      eol          = RESET + uncolored.eol,
      showChildren = uncolored.showChildren)
  }
}
