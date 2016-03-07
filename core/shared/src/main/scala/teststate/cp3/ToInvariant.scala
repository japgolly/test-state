package teststate.cp3

import acyclic.file
import Profunctor.ToOps._
import Types.CheckShapeA

trait ToInvariant[F[_[_, _], _, _], C[_, _]] {
  def toInvariant[A, B](c: F[C, A, B]): F[Invariant, A, B]
}

object ToInvariant {
  type Id[C[_, _], A, B] = C[A, B]

  implicit val pointToInvariant: ToInvariant[Id, Point] =
    new ToInvariant[Id, Point] {
      override def toInvariant[A, B](c: Point[A, B]) =
        Invariant.Point(c)
    }

  implicit val aroundToInvariant: ToInvariant[Id, Around] =
    new ToInvariant[Id, Around] {
      override def toInvariant[A, B](c: Around[A, B]) =
        Invariant.Around(c)
    }

  private def checksToInvariants[C[_, _]](implicit ci: ToInvariant[Id, C]): ToInvariant[CheckShapeA, C] =
    new ToInvariant[CheckShapeA, C] {
      override def toInvariant[A, B](c: CheckShapeA[C, A, B]): CheckShapeA[Invariant, A, B] =
        c.rmap(ci.toInvariant)
    }

  implicit val pointsToInvariants: ToInvariant[CheckShapeA, Point] =
    checksToInvariants[Point]

  implicit val aroundsToInvariants: ToInvariant[CheckShapeA, Around] =
    checksToInvariants[Around]

  implicit val invariantsToInvariant: ToInvariant[CheckShapeA, Invariant] =
    new ToInvariant[CheckShapeA, Invariant] {
      override def toInvariant[A, B](c: CheckShapeA[Invariant, A, B]) = c
    }
}

