package teststate.core

import teststate.core.Types.{CheckShapeA, checkShapeAProfunctorOps}

trait ToInvariants[F[_[-_, _], _, _], C[-_, _]] {
  def toInvariants[A, B](c: F[C, A, B]): F[Invariant, A, B]
}

object ToInvariants {
  type Id[C[-_, _], A, B] = C[A, B]

  implicit val pointToInvariant: ToInvariants[Id, Point] =
    new ToInvariants[Id, Point] {
      override def toInvariants[A, B](c: Point[A, B]) =
        Invariant.Point(c)
    }

  implicit val aroundToInvariant: ToInvariants[Id, Around] =
    new ToInvariants[Id, Around] {
      override def toInvariants[A, B](a: Around[A, B]) =
        a match {
          case Around.Point(p, _) => Invariant.Point(p)
          case Around.Delta(d)    => Invariant.Delta(d)
        }
    }

  private def checksToInvariants[C[-_, _]](implicit ci: ToInvariants[Id, C]): ToInvariants[CheckShapeA, C] =
    new ToInvariants[CheckShapeA, C] {
      override def toInvariants[A, B](c: CheckShapeA[C, A, B]): CheckShapeA[Invariant, A, B] =
        c.rmap(_ map ci.toInvariants)
    }

  implicit val pointsToInvariants: ToInvariants[CheckShapeA, Point] =
    checksToInvariants[Point]

  implicit val aroundsToInvariants: ToInvariants[CheckShapeA, Around] =
    checksToInvariants[Around]

  implicit val invariantsToInvariant: ToInvariants[CheckShapeA, Invariant] =
    new ToInvariants[CheckShapeA, Invariant] {
      override def toInvariants[A, B](c: CheckShapeA[Invariant, A, B]) = c
    }
}

