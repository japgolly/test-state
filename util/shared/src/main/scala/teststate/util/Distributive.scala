package teststate.util


trait Distributive[F[_]] extends Functor[F] {
  def distribute[G[_]: Functor, A, B](ga: G[A])(f: A => F[B]): F[G[B]]

  final def cosequence[G[_]:Functor,A](fa: G[F[A]]): F[G[A]] =
    distribute(fa)(identity)

  final def cotraverse[G[_]:Functor,A,B](gfa: G[F[A]])(f: G[A] => B): F[B] =
    map(cosequence(gfa))(f)
}

object Distributive {

  implicit lazy val fn0: Distributive[Function0] =
    new Distributive[Function0] {
      override def map[A, B](fa: () => A)(f: A => B): () => B =
        () => f(fa())
      override def distribute[G[_] : Functor, A, B](ga: G[A])(f: A => () => B): () => G[B] =
        () => implicitly[Functor[G]].map(ga)(f(_)())
    }

  implicit def fn1[I]: Distributive[I => *] =
    new Distributive[I => *] {
      override def map[A, B](fa: I => A)(f: A => B): I => B =
        f compose fa
      override def distribute[G[_] : Functor, A, B](ga: G[A])(f: A => I => B): I => G[B] =
        i => implicitly[Functor[G]].map(ga)(f(_)(i))
    }

  implicit def fn2[I, J]: Distributive[(I, J) => *] =
    new Distributive[(I, J) => *] {
      override def map[A, B](fa: (I, J) => A)(f: A => B): (I, J) => B =
        (i, j) => f(fa(i, j))
      override def distribute[G[_] : Functor, A, B](ga: G[A])(f: A => (I, J) => B): (I, J) => G[B] =
        (i, j) => implicitly[Functor[G]].map(ga)(f(_)(i, j))
    }

  implicit def fn3[I, J, K]: Distributive[(I, J, K) => *] =
    new Distributive[(I, J, K) => *] {
      override def map[A, B](fa: (I, J, K) => A)(f: A => B): (I, J, K) => B =
        (i, j, k) => f(fa(i, j, k))
      override def distribute[G[_] : Functor, A, B](ga: G[A])(f: A => (I, J, K) => B): (I, J, K) => G[B] =
        (i, j, k) => implicitly[Functor[G]].map(ga)(f(_)(i, j, k))
    }

}
