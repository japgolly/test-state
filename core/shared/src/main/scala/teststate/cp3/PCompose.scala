package teststate.cp3

trait PCompose[A, B, C] {
  def compose(a: A, b: B): C
}

object PCompose {
  type Mono[A] = PCompose[A, A, A]

  final class Ops[A](private val a: A) extends AnyVal {
    def &[B, C](b: B)(implicit c: PCompose[A, B, C]): C =
      c.compose(a, b)
  }

  trait LowPriInstances {
    import Types._

    implicit def checksPolyPCompose[C[_, _], D[_, _], A, E](implicit c: ToInvariant[CheckShapeA, C],
                                                                     d: ToInvariant[CheckShapeA, D],
                                                                     i: PCompose.Mono[CheckShapeA[Invariant, A, E]])
        : PCompose[CheckShapeA[C, A, E], CheckShapeA[D, A, E], CheckShapeA[Invariant, A, E]] = {
      type F[X[_, _]] = CheckShapeA[X, A, E]
      new PCompose[F[C], F[D], F[Invariant]] {
        override def compose(fc: F[C], fd: F[D]) =
          i.compose(c toInvariant fc, d toInvariant fd)
      }
    }
  }

  trait Instances extends LowPriInstances {
    import Types._

    implicit def checksMonoPCompose[A, B]: PCompose[Sack[A, B], Sack[A, B], Sack[A, B]] =
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

    implicit def checkPComposeAddOps[C[_, _], A, B]: AddOps[CheckShapeA[C, A, B]] =
      AddOps
  }

  /** Allows [[PCompose]] ops to be implicitly added to a type.
    *
    * Prevents [[PCompose]] ops from being added to all types when only a small subset have [[PCompose]] typeclasses.
    */
  sealed trait AddOps[A]
  @inline def AddOps[A] = null.asInstanceOf[AddOps[A]]

  trait ToOps {
    implicit def toPComposeOps[A: AddOps](a: A): Ops[A] =
      new Ops(a)
  }

  trait Implicits extends Instances with ToOps
}
