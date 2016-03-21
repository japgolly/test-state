package teststate.core

import acyclic.file
import teststate.data._
import teststate.typeclass._
import teststate.{core => ^}
import Conditional.Implicits._
import NamedOps.ToOps._
import Profunctor.ToOps._
import Show.ToOps._

final case class Point[-I, E](name: NameFn[I], test: I => Tri[E, Unit])

object Point {
  implicit val pointInstance: Profunctor[Point] =
    new Profunctor[Point] {
      override def dimap[A, B, C, D](p: Point[A, B])(g: C => A, f: B => D): Point[C, D] =
        Point(p.name cmap g, c => p.test(g(c)) mapE f)
    }

  implicit def pointInstanceConditional[I, E]: Conditional[Point[I, E], I] =
    Conditional((p, f) => Point(p.name, p.test when f))

  implicit def pointInstanceNamedOps[I, E]: NamedOps[Point[I, E], I] =
    NamedOps((p, f) => p.copy(name = f(p.name)))

  implicit def pointInstanceShow[I, E]: Show[Point[I, E]] =
    Show(_.name(None).value)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

sealed abstract class Around[-I, E]
object Around {

  sealed abstract class When
  case object Before extends When
  case object After  extends When

  final case class Point[-I, E](point: ^.Point[I, E], when: When) extends Around[I, E]

  // This is a wrapper around DeltaA because when DeltaA extends Around directly, scalac bugs cause it to sometimes be
  // omitted from pattern-matching exhaustiveness analysis.
  final case class Delta[-I, E](delta: DeltaA[I, E]) extends Around[I, E]

  type DeltaAux[-I, E, AA] = DeltaA[I, E] {type A = AA}

  sealed abstract class DeltaA[-I, E] {
    type A
    val name: NameFn[BeforeAfter[I]]
    val before: I => Tri[E, A]
    val test: (I, A) => Option[E]

    final def aux: DeltaAux[I, E, A] = this
  }

  def DeltaA[I, E, AA](_name: NameFn[BeforeAfter[I]], _before: I => Tri[E, AA], _test: (I, AA) => Option[E]): DeltaAux[I, E, AA] =
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
          d.name cmap (_ map g),
          c => b(g(c)) mapE f,
          (c, a) => t(g(c), a) map f)
      }
    }

  implicit def deltaAInstanceConditional[I, E]: Conditional[DeltaA[I, E], I] =
    Conditional((d, f) => DeltaA(d.name, d.before when f, d.test))

  implicit def deltaAInstanceNamedOps[I, E]: NamedOps[DeltaA[I, E], BeforeAfter[I]] =
    NamedOps((d, f) => DeltaA(f(d.name), d.before, d.test))

  implicit def deltaAInstanceShow[I, E]: Show[DeltaA[I, E]] =
    Show(_.name(None).value)

  implicit val aroundInstance: Profunctor[Around] =
    new Profunctor[Around] {
      override def dimap[A, B, C, D](around: Around[A, B])(g: C => A, f: B => D): Around[C, D] =
        around match {
          case Point(p, w) => Point(p.dimap(g, f), w)
          case Delta(d)    => Delta(d.dimap(g, f))
        }
    }

  implicit def aroundInstanceConditional[I, E]: Conditional[Around[I, E], I] =
    Conditional((m, f) => m match {
      case Point(p, w) => Point(p when f, w)
      case Delta(d)    => Delta(d when f)
    })

  implicit def aroundInstanceNamedOps[I, E]: NamedOps[Around[I, E], BeforeAfter[I]] =
    NamedOps((a, f) => a match {
      case Point(p, Before) => Point(p renameBy f.thruBefore, Before)
      case Point(p, After ) => Point(p renameBy f.thruAfter , After)
      case Delta(d)         => Delta(d renameBy f)
    })

  implicit def aroundInstanceShow[I, E]: Show[Around[I, E]] =
    Show {
      case Point(p, _) => p.show
      case Delta(d)    => d.show
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

sealed abstract class Invariant[-I, E]
object Invariant {
  final case class Point[-I, E](point: ^.Point        [I, E]) extends Invariant[I, E]
  final case class Delta[-I, E](delta: ^.Around.DeltaA[I, E]) extends Invariant[I, E]

  implicit val invariantInstanceProfunctor: Profunctor[Invariant] =
    new Profunctor[Invariant] {
      override def dimap[A, B, C, D](a: Invariant[A, B])(g: C => A, f: B => D): Invariant[C, D] =
        a match {
          case Point(x) => Point(x.dimap(g, f))
          case Delta(x) => Delta(x.dimap(g, f))
        }
    }

  implicit def invariantInstanceConditional[I, E]: Conditional[Invariant[I, E], I] =
    Conditional((m, f) => m match {
      case Point(x) => Point(x when f)
      case Delta(x) => Delta(x when f)
    })

  implicit def invariantInstanceNamedOps[I, E]: NamedOps[Invariant[I, E], I] =
    NamedOps((i, f) => i match {
      case Point(x) => Point(x renameBy f)
      case Delta(x) =>
        // Hmmmm......
        Delta(x renameBy (n => f(n.cmap(BeforeAfter.same)).cmap(_.before)))
    })

  implicit def invariantInstanceShow[I, E]: Show[Invariant[I, E]] =
    Show {
      case Point(x) => x.show
      case Delta(x) => x.show
    }
}

