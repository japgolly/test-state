package teststate

sealed abstract class Check[O, S, +E] {
  type This[o, s, +e] <: Check[o, s, e]

  final def &[e >: E](c: Check[O, S, e]): Check.Composite[O, S, e] =
    Check.Composite(getPoint & c.getPoint, getAround & c.getAround)

  def getPoint : Check.Point[O, S, E]
  def getAround: Check.Around[O, S, E]

  final def skip[EE >: E]: This[O, S, E] =
    when(_ => false)

  def when[EE >: E](f: OS[O, S] => Boolean): This[O, S, E]

  final def cmapO[OO](o: OO => O): This[OO, S, E] =
    cmap(o, identity)

  final def cmapS[SS](s: SS => S): This[O, SS, E] =
    cmap(identity, s)

  def cmap[OO, SS](o: OO => O, s: SS => S): This[OO, SS, E]

  def mapE[EE](f: E => EE): This[O, S, EE]

  def pmapO[OO, EE >: E](o: OO => EE Or O): This[OO, S, EE]
}

object Check {

  def empty[O, S] = Composite[O, S, Nothing](Point.empty, Around.empty)

  final case class Composite[O, S, +E](getPoint: Point[O, S, E], getAround: Around[O, S, E]) extends Check[O, S, E] {
    override type This[o, s, +e] = Composite[o, s, e]

    override def when[EE >: E](f: OS[O, S] => Boolean) = Composite(
      getPoint when f,
      getAround when f)

    override def cmap[OO, SS](o: OO => O, s: SS => S) = Composite(
      getPoint.cmap(o, s),
      getAround.cmap(o, s))

    override def pmapO[OO, EE >: E](f: OO => EE Or O) = Composite(
      getPoint pmapO f,
      getAround pmapO f)

    override def mapE[EE](f: E => EE) = Composite(
      getPoint mapE f,
      getAround mapE f)
  }

  // ===================================================================================================================

  sealed abstract class Point[O, S, +E] extends Check[O, S, E] {
    override type This[o, s, +e] <: Point[o, s, e]

    def getSingles: Vector[Point.Single[O, S, E]]
    def before: Around[O, S, E]
    def after : Around[O, S, E]

    final def beforeAndAfter: Around.Composite[O, S, E] =
      before & after

    override final def getPoint  = this
    override final def getAround = Around.empty[O, S]

    final def &[e >: E](c: Point[O, S, e]): Point.Composite[O, S, e] =
      Point.Composite(getSingles ++ c.getSingles)
  }

  object Point {
    def empty[O, S] = Composite[O, S, Nothing](Vector.empty)

    final case class Composite[O, S, +E](getSingles: Vector[Single[O, S, E]]) extends Point[O, S, E] {
      override type This[o, s, +e] = Composite[o, s, e]
      override def before = Around.Composite(getSingles.map(_.before), Vector.empty, Vector.empty)
      override def after  = Around.Composite(Vector.empty, Vector.empty, getSingles.map(_.after))

      override def when[EE >: E](f: OS[O, S] => Boolean) = Composite(
        getSingles.map(_ when f))

      override def cmap[OO, SS](o: OO => O, s: SS => S) = Composite(
        getSingles.map(_.cmap(o, s)))

      override def pmapO[OO, EE >: E](f: OO => EE Or O) = Composite(
        getSingles.map(_ pmapO f))

      override def mapE[EE](f: E => EE) = Composite(
        getSingles.map(_ mapE f))
    }

    final case class Single[O, S, +E](name: NameFn[OS[O, S]], test: OS[O, S] => TriResult[E, Unit]) extends Point[O, S, E] {
      override type This[o, s, +e] = Single[o, s, e]
      override def toString = s"Check.Point.Single(${name(None)})"
      override def getSingles = vector1(this)
      override def before = Around.Before(this)
      override def after  = Around.After(this)
      def rename[e >: E](newName: NameFn[OS[O, S]]): Single[O, S, e] =
        copy(newName)

      override def when[EE >: E](f: OS[O, S] => Boolean) = Single(
        name,
        wrapWithCond2(f, test))

      override def cmap[OO, SS](o: OO => O, s: SS => S) = Single[OO, SS, E](
        name.cmap(_.map(o, s)),
        as => test(as.map(o, s)))

      override def pmapO[OO, EE >: E](f: OO => EE Or O) = Single[OO, S, EE](
        name.comap(_ mapOe f),
        _.mapOE(f).toTriResultFlatMap(test))

      override def mapE[EE](f: E => EE) = Single[O, S, EE](
        name,
        test(_) mapE f)
    }
  }

  // ===================================================================================================================

  sealed abstract class Around[O, S, +E] extends Check[O, S, E] {
    override type This[o, s, +e] <: Around[o, s, e]

    def getBefores: Vector[Around.Before[O, S, E]]
    def getDunnos: Vector[Around.Dunno[O, S, E]]
    def getAfters: Vector[Around.After[O, S, E]]

    override final def getPoint  = Point.empty[O, S]
    override final def getAround = this

    final def &[e >: E](c: Around[O, S, e]): Around.Composite[O, S, e] =
      Around.Composite(
        getBefores ++ c.getBefores,
        getDunnos ++ c.getDunnos,
        getAfters ++ c.getAfters)
  }

  object Around {
    def empty[O, S] = Composite[O, S, Nothing](Vector.empty, Vector.empty, Vector.empty)

    final case class Composite[O, S, +E](getBefores: Vector[Before[O, S, E]],
                                         getDunnos: Vector[Dunno[O, S, E]],
                                         getAfters: Vector[After[O, S, E]]) extends Around[O, S, E] {
      override type This[o, s, +e] = Composite[o, s, e]

      override def when[EE >: E](f: OS[O, S] => Boolean) = Composite(
        getBefores.map(_ when f),
        getDunnos.map(_ when f),
        getAfters.map(_ when f))

      override def cmap[OO, SS](o: OO => O, s: SS => S) = Composite(
        getBefores.map(_.cmap(o, s)),
        getDunnos.map(_.cmap(o, s)),
        getAfters.map(_.cmap(o, s)))

      override def pmapO[OO, EE >: E](f: OO => EE Or O) = Composite(
        getBefores.map(_ pmapO f),
        getDunnos.map(_ pmapO f),
        getAfters.map(_ pmapO f))

      override def mapE[EE](f: E => EE) = Composite(
        getBefores.map(_ mapE f),
        getDunnos.map(_ mapE f),
        getAfters.map(_ mapE f))
    }

    sealed abstract class Single[O, S, +E] extends Around[O, S, E] {
      override type This[o, s, +e] <: Single[o, s, e]

      override def getBefores: Vector[Before[O, S, E]] = Vector.empty
      override def getDunnos: Vector[Dunno[O, S, E]] = Vector.empty
      override def getAfters: Vector[After[O, S, E]] = Vector.empty

      def rename[e >: E](newName: NameFn[OS[O, S]]): This[O, S, e]
    }

    sealed abstract class Dunno[O, S, +E] extends Single[O, S, E] {
      type A
      val name: NameFn[OS[O, S]]
      val before: OS[O, S] => TriResult[E, A]
      val test: (OS[O, S], A) => Option[E]

      final def aux: DunnoA[O, S, E, A] = this
      final override type This[o, s, +e] = DunnoA[o, s, e, A]
      final override def getDunnos = vector1(this)

      final override def when[EE >: E](f: OS[O, S] => Boolean) = Dunno[O, S, E, A](
        name,
        wrapWithCond2(f, before),
        test)

      final override def cmap[OO, SS](o: OO => O, s: SS => S) = Dunno[OO, SS, E, A](
        name.cmap(_.map(o, s)),
        xs => before(xs.map(o, s)),
        (xs, a) => test(xs.map(o, s), a))

      final override def mapE[EE](f: E => EE) = Dunno[O, S, EE, A](
        name,
        before(_) mapE f,
        test(_, _) map f)

      final override def pmapO[OO, EE >: E](f: OO => EE Or O) = Dunno[OO, S, EE, A](
        name.comap(_ mapOe f),
        _.mapOE(f).toTriResultFlatMap(before),
        (xs, a) => xs.mapOE(f).test(test(_, a)).leftOption)
    }

    type DunnoA[O, S, +E, a] = Dunno[O, S, E] {type A = a}

    def Dunno[O, S, E, _A](_name: NameFn[OS[O, S]],
                            _before: OS[O, S] => TriResult[E, _A],
                            _test: (OS[O, S], _A) => Option[E]): DunnoA[O, S, E, _A] =
      new Dunno[O, S, E] {
        override type A     = _A
        override val name   = _name
        override val before = _before
        override val test   = _test
        override def toString = s"Check.Around.Single(${name(None)})"
        override def rename[e >: E](newName: NameFn[OS[O, S]]) =
          Dunno(newName, before, test)
      }

    final case class Before[O, S, +E](check: Point.Single[O, S, E]) extends Single[O, S, E] {
      override type This[o, s, +e] = Before[o, s, e]
      override def getBefores = vector1(this)
      override def when[EE >: E](f: OS[O, S] => Boolean) = Before(check when f)
      override def cmap[OO, SS](o: OO => O, s: SS => S) = Before(check.cmap(o, s))
      override def mapE[EE](f: E => EE) = Before(check mapE f)
      override def pmapO[OO, EE >: E](f: OO => EE Or O) = Before(check pmapO f)
      override def rename[e >: E](newName: NameFn[OS[O, S]]) =
        copy(check rename newName)
    }

    final case class After[O, S, +E](check: Point.Single[O, S, E]) extends Single[O, S, E] {
      override type This[o, s, +e] = After[o, s, e]
      override def getAfters = vector1(this)
      override def when[EE >: E](f: OS[O, S] => Boolean) = After(check when f)
      override def cmap[OO, SS](o: OO => O, s: SS => S) = After(check.cmap(o, s))
      override def mapE[EE](f: E => EE) = After(check mapE f)
      override def pmapO[OO, EE >: E](f: OO => EE Or O) = After(check pmapO f)
      override def rename[e >: E](newName: NameFn[OS[O, S]]) =
        copy(check rename newName)
    }
  }
}
