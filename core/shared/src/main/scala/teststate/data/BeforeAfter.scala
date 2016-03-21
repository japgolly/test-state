package teststate.data

final case class BeforeAfter[+A](before: A, after: A) {
  def map[B](f: A => B): BeforeAfter[B] =
    BeforeAfter(f(before), f(after))

  def emap[E, B](f: A => E Or B): E Or BeforeAfter[B] =
    for {
      b <- f(before)
      a <- f(after)
    } yield BeforeAfter(b, a)
}

object BeforeAfter {
  def same[A](a: A): BeforeAfter[A] =
    BeforeAfter(a, a)
}