package teststate.cp3

trait PCompose[A, B, C] {
  def compose(a: A, b: B): C
}




import Types._

trait LP {

  trait ToInvariant[F[_[_, _], _, _], C[_, _]] {
    def toInvariant[A, B](c: F[C, A, B]): F[Invariant, A, B]
  }

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

  import Profunctor.ToOps._

  implicit def sackToInvariant[C[_, _]](implicit ci: ToInvariant[Id, C]): ToInvariant[CheckShapeA, C] =
    new ToInvariant[CheckShapeA, C] {
      override def toInvariant[A, B](c: CheckShapeA[C, A, B]): CheckShapeA[Invariant, A, B] =
        c.rmap(ci.toInvariant)
    }

  implicit val invariantSackToInvariant: ToInvariant[CheckShapeA, Invariant] =
    new ToInvariant[CheckShapeA, Invariant] {
      override def toInvariant[A, B](c: CheckShapeA[Invariant, A, B]) = c
    }

  //  implicit def sackToInvariant[C[_, _], A, E](s: CheckShapeA[C, A, E])(implicit ci: ToInvariant[C]): CheckShapeA[Invariant, A, E] =
  //    s.rmap(f)

  implicit def composeSackPoly[C[_, _], D[_, _], A, E](implicit ci: ToInvariant[CheckShapeA, C],
                                                       di: ToInvariant[CheckShapeA, D],
                                                       zzz: PCompose.Mono[CheckShapeA[Invariant, A, E]])
      : PCompose[CheckShapeA[C, A, E], CheckShapeA[D, A, E], CheckShapeA[Invariant, A, E]] = {
    type X = CheckShapeA[C, A, E]
    type Y = CheckShapeA[D, A, E]
    type Z = CheckShapeA[Invariant, A, E]
    new PCompose[X, Y, Z] {
      override def compose(x: CheckShapeA[C, A, E], y: Y): Z =
        zzz.compose(ci toInvariant x, di toInvariant y)
    }
  }
}

object PCompose extends LP {
  type Mono[A] = PCompose[A, A, A]

  implicit class PCOps[A](private val a: A) extends AnyVal {
    def &[B, C](b: B)(implicit c: PCompose[A, B, C]): C =
      c.compose(a, b)
  }

  implicit def composeSackMono[A, B]: PCompose[Sack[A, B], Sack[A, B], Sack[A, B]] =
    new PCompose[Sack[A, B], Sack[A, B], Sack[A, B]] {
      import Sack._
      override def compose(a: Sack[A, B], b: Sack[A, B]) =
        (a, b) match {
          case (Product(p), Product(q)) => Product(p ++ q)
          case (p         , Product(q)) => Product(p +: q)
          case (Product(p), q         ) => Product(p :+ q)
          case (p         , q         ) => Product(Vector.empty :+ p :+ q)
        }
    }


//  implicit def autosdlasdfP[O, S, E](p: Points[O, S, E]): Invariants[O, S, E] =
//    sackToInvariant[Point].toInvariant(p)
//
//  implicit def autosdlasdfA[O, S, E](p: Arounds[O, S, E]): Invariants[O, S, E] =
//    sackToInvariant[Around].toInvariant(p)

  implicit def autosdlasdfA[C[_, _], A, E](p: CheckShapeA[C, A, E])(implicit ci: ToInvariant[Id, C]): CheckShapeA[Invariant, A, E] =
    sackToInvariant[C].toInvariant(p)
}
