package teststate.typeclass


trait Profunctor[M[_, _]] {

  def lmap[A, B, C](m: M[A, B])(g: C => A): M[C, B] =
    dimap(m)(g, identity)

  def rmap[A, B, C](m: M[A, B])(f: B => C): M[A, C] =
    dimap(m)(identity, f)

  def dimap[A, B, C, D](m: M[A, B])(g: C => A, f: B => D): M[C, D] =
    rmap(lmap(m)(g))(f)
}

object Profunctor {

  final class Ops[M[_, _], A, B](m: M[A, B])(implicit p: Profunctor[M]) {

    def lmap[C](g: C => A): M[C, B] =
      p.lmap(m)(g)

    def rmap[C](f: B => C): M[A, C] =
      p.rmap(m)(f)

    def dimap[C, D](g: C => A, f: B => D): M[C, D] =
      p.dimap(m)(g, f)
  }

  trait ToOps {
    implicit def toProfunctorOps[M[_, _], A, B](mab: M[A, B])(implicit p: Profunctor[M]): Ops[M, A, B] =
      new Ops[M, A, B](mab)(p)
  }

  object ToOps extends ToOps
}

