package teststate.util

import acyclic.file

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  implicit lazy val option: Functor[Option] =
    new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B) = fa map f
    }

  implicit lazy val list: Functor[List] =
    new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B) = fa map f
    }

  implicit lazy val vector: Functor[Vector] =
    new Functor[Vector] {
      override def map[A, B](fa: Vector[A])(f: A => B) = fa map f
    }
}