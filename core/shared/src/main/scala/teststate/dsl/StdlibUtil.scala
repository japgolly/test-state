package teststate.dsl

import acyclic.file
import teststate.data.Or
import teststate.typeclass.{Distributive, Functor}
import StdlibUtil._

trait StdlibUtil {

  implicit def toStateTestEitherStringExt[A](e: Either[String, A]): StateTestEitherStringExt[A] =
    new StateTestEitherStringExt(e)

  implicit def testStateOrFromScalaEither[A, B](e: A Either B): A Or B =
    Or fromScalaEither e

  implicit def TestStateOptionExt[A](a: Option[A]): TestStateOptionExt[A] =
    new TestStateOptionExt(a)

  type NamedOption[+A] = StdlibUtil.NamedOption[A]

  implicit def toTestStateTraversableExt[A](as: Traversable[A]): TestStateTraversableExt[A] =
    new TestStateTraversableExt(as)

  type NamedVector[+A] = StdlibUtil.NamedVector[A]

  implicit def TestStateMapExt[K, V](a: Map[K, V]): TestStateMapExt[K, V] =
    new TestStateMapExt(a)

  type NamedMap[K, +V] = StdlibUtil.NamedMap[K, V]

}

object StdlibUtil {

  private implicit def toStateTestEitherStringExt[A](e: Either[String, A]): StateTestEitherStringExt[A] =
    new StateTestEitherStringExt(e)

  final class StateTestEitherStringExt[A](private val self: Either[String, A]) extends AnyVal {
    def getOrThrow(): A =
      self match {
        case Right(a) => a
        case Left(e) => sys.error(e)
      }
  }

  // ===================================================================================================================

  final class NamedOption[+A](val name: String, val underlying: Option[A]) {
    private def errMsg = s"$name not available"

    def filter(desc: String, f: A => Boolean): NamedOption[A] =
      new NamedOption(s"$name($desc)", underlying filter f)

    def get: A =
      underlying match {
        case Some(a) => a
        case None => sys.error(errMsg + ". None.get")
      }

    def attempt: Either[String, A] =
      underlying match {
        case Some(a) => Right(a)
        case None => Left(errMsg)
      }

    def map[B](f: A => B): NamedOption[B] =
      new NamedOption[B](name, underlying map f)

    def dist[F[_], B](implicit ev: NamedOption[A] <:< NamedOption[F[B]], F: Distributive[F]): F[NamedOption[B]] =
      F.cosequence(this)

    def distGet[F[_], B](implicit ev: NamedOption[A] <:< NamedOption[F[B]], F: Distributive[F]): F[B] =
      F.cotraverse(ev(this))(_.get)
  }

  object NamedOption {
    implicit def toOption[A](n: NamedOption[A]): Option[A] = n.underlying

    implicit lazy val functor: Functor[NamedOption] =
      new Functor[NamedOption] {
        override def map[A, B](fa: NamedOption[A])(f: A => B) = fa map f
      }
  }

  final class TestStateOptionExt[A](private val self: Option[A]) extends AnyVal {
    def named(name: String): NamedOption[A] =
      new NamedOption(name, self)
  }

  // ===================================================================================================================

  final class NamedVector[+A](namePlural: String, val underlying: Vector[A]) {

    def filter(desc: String, f: A => Boolean): NamedVector[A] =
      new NamedVector(s"$namePlural($desc)", underlying filter f)

    /** Expect exactly one element */
    def attemptOne: Either[String, A] =
      underlying.length match {
        case 1 => Right(underlying.head)
        case n => Left(s"$n $namePlural found. Expect exactly 1.")
      }

    def attemptAtIndex(i: Int): Either[String, A] =
      if (underlying.indices.contains(i))
        Right(underlying(i))
      else
        Left(s"$namePlural[$i] not found; ${underlying.length} available.")

    def map[B](f: A => B): NamedVector[B] =
      new NamedVector[B](namePlural, underlying map f)

    /** Expect exactly one element */
    def getOne: A =
      attemptOne.getOrThrow()

    def getAtIndex(i: Int): A =
      attemptAtIndex(i).getOrThrow()

    def dist[F[_], B](implicit ev: NamedVector[A] <:< NamedVector[F[B]], F: Distributive[F]): F[NamedVector[B]] =
      F.cosequence(this)

    def distGetOne[F[_], B](implicit ev: NamedVector[A] <:< NamedVector[F[B]], F: Distributive[F]): F[B] =
      F.cotraverse(ev(this))(_.getOne)
  }

  object NamedVector {
    implicit def toVector[A](n: NamedVector[A]): Vector[A] = n.underlying

    implicit lazy val functor: Functor[NamedVector] =
      new Functor[NamedVector] {
        override def map[A, B](fa: NamedVector[A])(f: A => B) = fa map f
      }
  }

  final class TestStateTraversableExt[A](private val self: Traversable[A]) extends AnyVal {
    def named(namePlural: String): NamedVector[A] =
      new NamedVector(namePlural, self.toVector)
  }


  // ===================================================================================================================

  final class NamedMap[K, +V](namePlural: String, val underlying: Map[K, V]) {

    def toVector: NamedVector[(K, V)] =
      new NamedVector(namePlural, underlying.toVector)

    def filter(desc: String, f: ((K, V)) => Boolean): NamedMap[K, V] =
      new NamedMap(s"$namePlural($desc)", underlying filter f)

    def get(k: K): NamedOption[V] =
      new NamedOption(namePlural, underlying.get(k))

    def apply(k: K): V =
      underlying.get(k) match {
        case Some(v) => v
        case None => sys.error(s"$namePlural doesn't contain $k; it contains ${underlying.keys.mkString("[", ", ", "]")}")
      }
  }

  implicit def NamedMapToMap[K, V](n: NamedMap[K, V]): Map[K, V] = n.underlying

  final class TestStateMapExt[K, V](private val self: Map[K, V]) extends AnyVal {
    def named(namePlural: String): NamedMap[K, V] =
      new NamedMap(namePlural, self.toMap)
  }

}