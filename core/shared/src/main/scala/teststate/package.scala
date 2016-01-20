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

  case class History[+Err, +A](steps: History.Steps[Err, A]) {
    def failure: Option[Err] = {
      val it = steps.iterator.map(_.result.failure).filter(_.isDefined)
      if (it.hasNext) it.next() else None
    }
  }

  object History {
    type Steps[+Err, +A] = Vector[Step[Err, A]]
    case class Step[+Err, +A](indent: Int, name: String, result: Result[Err], arb: A)
  }

}
