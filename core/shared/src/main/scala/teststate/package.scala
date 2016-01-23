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
    def leftOrF[L >: A, R](e: => Either[L, R]): Either[L, R] =
      self match {
        case None    => e
        case Some(a) => Left(a)
      }

    @inline def leftOr[L >: A, R](r: => R): Either[L, R] =
      leftOrF(Right(r))
  }

  implicit class TestStateExtMethodsForEither[A, B](private val self: Either[A, B]) extends AnyVal {
    def fmap[C](f: B => Either[A, C]): Either[A, C] =
      self match {
        case Right(b) => f(b)
        case l: Left[A, B] => l.asInstanceOf[Left[A, Nothing]]
      }

    @inline def map[C](f: B => C): Either[A, C] =
      fmap(b => Right(f(b)))

    def leftMap[C](f: A => C): Either[C, B] =
      self match {
        case r: Right[A, B] => r.asInstanceOf[Right[Nothing, B]]
        case Left(a) => Left(f(a))
      }

    def check[C](f: B => Option[A]): Either[A, B] =
      self match {
        case r: Right[A, B] => f(r.b).leftOrF(r) //.fold[Either[A, B]](r)(Left(_))
        case l: Left[A, B]  => l
      }

    def bimap[C, D](f: A => C, g: B => D): Either[C, D] =
      self match {
        case Right(b) => Right(g(b))
        case Left(a) => Left(f(a))
      }
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

    def failed = failure.isDefined
  }

  case class Show[A](show: A => String) extends AnyVal {
    def apply(a: A): String =
      // Handle \n, \t, spaces (so surrounds), long strings
      "[" + show(a) + "]"

  }
  object Show {
    implicit val showString: Show[String] = Show(identity)
    implicit val showInt: Show[Int] = Show(_.toString)
  }

  implicit def focusDslb2ToCheck[O, S, E, A](b: FocusDsl[O, S, E]#B2[A]) = b.check

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
