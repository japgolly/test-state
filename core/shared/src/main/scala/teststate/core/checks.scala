package teststate.core

import acyclic.file
import teststate.data._
import teststate.typeclass._
import teststate.{core => ^}
import Profunctor.ToOps._
import Conditional.Implicits._

final case class Point[I, E](name: NameFn[I], test: I => Tri[E, Unit])

object Point {
  implicit val pointInstance: Profunctor[Point] =
    new Profunctor[Point] {
      override def dimap[A, B, C, D](p: Point[A, B])(g: C => A, f: B => D): Point[C, D] =
        Point(p.name cmap g, c => p.test(g(c)) mapE f)
    }

  implicit def pointInstanceConditional[I, E]: Conditional[Point[I, E], I] =
    new Conditional[Point[I, E], I] {
      override def when(m: Point[I, E], f: I => Boolean) =
        Point(m.name, m.test when f)
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

sealed abstract class Around[I, E]
object Around {

  sealed abstract class When
  case object Before         extends When
  case object After          extends When
  case object BeforeAndAfter extends When

  final case class Point[I, E](point: ^.Point[I, E], when: When) extends Around[I, E]

  // This is a wrapper around DeltaA because when DeltaA extends Around directly, scalac bugs cause it to sometimes be
  // omitted from pattern-matching exhaustiveness analysis.
  final case class Delta[I, E](delta: DeltaA[I, E]) extends Around[I, E]

  type DeltaAux[I, E, AA] = DeltaA[I, E] {type A = AA}

  sealed abstract class DeltaA[I, E] {
    type A
    val name: NameFn[I]
    val before: I => Tri[E, A]
    val test: (I, A) => Option[E]

    final def aux: DeltaAux[I, E, A] = this
  }

  def DeltaA[I, E, AA](_name: NameFn[I], _before: I => Tri[E, AA], _test: (I, AA) => Option[E]): DeltaAux[I, E, AA] =
    new DeltaA[I, E] {
      override type A     = AA
      override val name   = _name
      override val before = _before
      override val test   = _test
    }

  implicit val deltaAInstance: Profunctor[DeltaA] =
    new Profunctor[DeltaA] {
      override def dimap[A, B, C, D](d: DeltaA[A, B])(g: C => A, f: B => D): DeltaA[C, D] = {
        val b = d.before
        val t = d.test
        DeltaA[C, D, d.A](
          d.name cmap g,
          c => b(g(c)) mapE f,
          (c, a) => t(g(c), a) map f)
      }
    }

  implicit def deltaAInstanceConditional[I, E]: Conditional[DeltaA[I, E], I] =
    new Conditional[DeltaA[I, E], I] {
      override def when(d: DeltaA[I, E], f: I => Boolean) =
        DeltaA(d.name, d.before when f, d.test)
    }

  implicit val aroundInstance: Profunctor[Around] =
    new Profunctor[Around] {
      override def dimap[A, B, C, D](around: Around[A, B])(g: C => A, f: B => D): Around[C, D] =
        around match {
          case Point(p, w) => Point(p.dimap(g, f), w)
          case Delta(d)    => Delta(d.dimap(g, f))
        }
    }

  implicit def aroundInstanceConditional[I, E]: Conditional[Around[I, E], I] =
    new Conditional[Around[I, E], I] {
      override def when(m: Around[I, E], f: I => Boolean) =
        m match {
          case Point(p, w) => Point(p when f, w)
          case Delta(d)    => Delta(d when f)
        }
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

sealed abstract class Invariant[I, E]
object Invariant {
  final case class Point[I, E](point: ^.Point        [I, E]) extends Invariant[I, E]
  final case class Delta[I, E](delta: ^.Around.DeltaA[I, E]) extends Invariant[I, E]

  implicit val invariantInstanceProfunctor: Profunctor[Invariant] =
    new Profunctor[Invariant] {
      override def dimap[A, B, C, D](a: Invariant[A, B])(g: C => A, f: B => D): Invariant[C, D] =
        a match {
          case Point(x) => Point(x.dimap(g, f))
          case Delta(x) => Delta(x.dimap(g, f))
        }
    }

  implicit def invariantInstanceConditional[I, E]: Conditional[Invariant[I, E], I] =
    new Conditional[Invariant[I, E], I] {
      override def when(m: Invariant[I, E], f: I => Boolean) =
        m match {
          case Point(x) => Point(x when f)
          case Delta(x) => Delta(x when f)
        }
    }
}

