package teststate.domzipper

final case class MofN(m: Int, n: Int) {
  override def toString = s"$m of $n"
  assert(n > 0, s"$this is invalid. $n must be > 0.")
  assert(m > 0, s"$this is invalid. $m must be > 0.")
  assert(m <= n, s"$this is invalid. $m must be ≤ $n.")
}

object MofN {
  val Sole = MofN(1, 1)

  final class IntExt(private val i: Int) extends AnyVal {
    def of(n: Int) = MofN(i, n)
  }
}
