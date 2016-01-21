package object teststate {

  @inline private[teststate] def vector1[A](a: A): Vector[A] =
    Vector.empty[A] :+ a

  /*
  case class Plan[State, Obj, Err](steps: Plan.Steps[State, Obj, Err]) {
    def andThen(next: Plan[State, Obj, Err]): Plan[State, Obj, Err] =
      Plan(steps ++ next.steps)
  }

  object Plan {
    type Steps[State, Obj, Err] = Vector[Step[State, Obj, Err]]
    case class Step[State, Obj, Err](indent: Int, name: String, action: Action.NonComposite[State, Obj, Err])
  }
  */

  implicit class TestStateExtMethodsForOption[A](private val self: Option[A]) extends AnyVal {
    def leftBind[L >: A, R](e: => Either[L, R]): Either[L, R] =
      self match {
        case None    => e
        case Some(a) => Left(a)
      }

    @inline def leftOr[L >: A, R](r: => R): Either[L, R] =
      leftBind(Right(r))
  }

  implicit class TestStateExtMethodsForEither[L, A](private val self: Either[L, A]) extends AnyVal {
    def fmap[B](f: A => Either[L, B]): Either[L, B] =
      self match {
        case Right(a) => f(a)
        case l: Left[L, A] => l.asInstanceOf[Left[L, Nothing]]
      }

    @inline def map[B](f: A => B): Either[L, B] =
      fmap(a => Right(f(a)))
  }

  case class History[+E](steps: History.Steps[E]) {

    val result: Result[E] = {
      var skipSeen = false
      var firstError: Option[Result.Fail[E]] = None
      steps foreach (_.result match {
        case Result.Pass => ()
        case Result.Skip => skipSeen = true
        case e: Result.Fail[E] => if (firstError.isEmpty) firstError = Some(e)
      })
      firstError.getOrElse(if (skipSeen) Result.Skip else Result.Pass)
    }

    def failure: Option[E] =
      result.failure
  }

  case class Show[E](show: E => String) extends AnyVal
  object Show {
    implicit val showString: Show[String] = Show(identity)
  }

  object History {
    val empty = History(Vector.empty)
    type Steps[+Err] = Vector[Step[Err]]
    case class Step[+E](name: String, result: Result[E], children: History[E] = empty) {
      def failure = result.failure
      def failed = failure.isDefined
    }

    def parent[E](name: String, children: History[E]): Step[E] =
      Step(name, children.result, children)
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
    import scala.Console._

    val colored = Options(
      indent       = "  ",
      onPass       = BOLD + GREEN + "✓" + RESET + WHITE, // ✓ ✔
      onSkip       = BOLD + YELLOW + "↓" + BLACK, // ⇣ ↶ ↷
      onFail       = BOLD + RED + "✘" + RESET + RED, // ✗ ✘
      eol          = RESET + "\n",
      showChildren = _.failure.isDefined)
  }

  def formatHistory[E](history: History[E], options: Options)(implicit showError: Show[E]): String = {
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

    def showSteps(steps: History.Steps[E], indent: Int): Unit =
      steps foreach (showStep(_, indent))

    def showStep(step: History.Step[E], indent: Int): Unit = {
      val showChildren = step.children.steps.nonEmpty && options.showChildren(step.children)
      val error = if (showChildren) None else step.result.failure

      appendIndent(indent)
      appendResultFlag(step.result)
      sb append ' '
      sb append step.name
      for (e <- error) {
        sb append " -- "
        sb append showError.show(e)
      }
      sb append options.eol

      if (showChildren)
        showHistory(step.children, indent + 1)
    }

    showHistory(history, 0)

    sb.result()
  }
}
