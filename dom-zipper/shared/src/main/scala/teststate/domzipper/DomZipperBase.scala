package teststate.domzipper

import DomZipper._
import DomZipperBase._
import ErrorHandler.Id

trait DomZipperBase[F[_], Dom, A, Self[G[_], B] <: DomZipperBase[G, Dom, B, Self]] extends DomZipper[F, Dom, A, Self] {
  import DomCollection.Container

  final override def describe: String =
    s"DESC: ${layers.all.iterator.map(_.display) mkString " -> "}\nHTML: $outerHTML"

  // ==================
  // Self configuration
  // ==================

  protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]): Self[G, A]

  protected implicit val $: CssSelEngine[Dom, Dom]

  /** oldest to newest; furthest to closest. */
  protected val layers: DomZipperBase.Layers[Dom]

  protected[domzipper] def addLayer(nextLayer: Layer[Dom]): Self[F, A]

  final override def scrubHtml(f: HtmlScrub): Self[F, A] =
    copySelf(htmlScrub >> f, F)

  final def failBy[G[_]](g: ErrorHandler[G]): Self[G, A] =
    copySelf(htmlScrub, g)

  final def failToOption: Self[Option           , A] = failBy(ErrorHandler.ReturnOption)
  final def failToEither: Self[Either[String, ?], A] = failBy(ErrorHandler.ReturnEither)
  final def throwErrors : Self[Id               , A] = failBy(ErrorHandler.Throw)

  // ====================
  // DOM & DOM inspection
  // ====================

  protected def _outerHTML: String
  protected def _innerHTML: String

  protected def collect        [C[_]](sel: String, C: Container[F, C])              : DomCollection[Self, F, C, Dom, A]
  protected def collectChildren[C[_]](desc: String, C: Container[F, C])             : DomCollection[Self, F, C, Dom, A]
  protected def collectChildren[C[_]](desc: String, sel: String, C: Container[F, C]): DomCollection[Self, F, C, Dom, A]

  final override def dom: Dom = layers.latest.dom

  final override def collect01(sel: String): DomCollection[Self, F, Option, Dom, A] = collect(sel, F.C01)
  final override def collect0n(sel: String): DomCollection[Self, F, Vector, Dom, A] = collect(sel, F.C0N)
  final override def collect1n(sel: String): DomCollection[Self, F, Vector, Dom, A] = collect(sel, F.C1N)

  final override def children01: DomCollection[Self, F, Option, Dom, A] = collectChildren(">*", F.C01)
  final override def children0n: DomCollection[Self, F, Vector, Dom, A] = collectChildren(">*", F.C0N)
  final override def children1n: DomCollection[Self, F, Vector, Dom, A] = collectChildren(">*", F.C1N)

  final override def children01(sel: String): DomCollection[Self, F, Option, Dom, A] = collectChildren(cssPrepend_>(sel), sel, F.C01)
  final override def children0n(sel: String): DomCollection[Self, F, Vector, Dom, A] = collectChildren(cssPrepend_>(sel), sel, F.C0N)
  final override def children1n(sel: String): DomCollection[Self, F, Vector, Dom, A] = collectChildren(cssPrepend_>(sel), sel, F.C1N)

  // =======
  // Descent
  // =======

  protected def _parent: F[Dom]

  final lazy val parent: F[Self[F, A]] =
    F.map(_parent)(dom => addLayer(Layer("parent", ":parent", dom)))

  final protected def runCssQuery(sel: String): CssSelResult[Dom] =
    $.run(sel, dom)

  final override def apply(name: String, sel: String, which: MofN): F[Self[F, A]] = {
    val results = runCssQuery(sel)
    if (results.length != which.n)
      F fail {
        val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
        failMsg(s"${q}uery failed: [$sel]. Expected ${which.n} results, not ${results.length}.")
      }
    else
      F pass {
        val nextDom = results(which.m - 1)
        val nextLayer = Layer(name, sel, nextDom)
        addLayer(nextLayer)
      }
  }

  final override def child(name: String, sel: String, which: MofN): F[Self[F, A]] = {
    val results = if (sel.isEmpty) children1n else children1n(sel)
    F.flatMap(results.zippers) { zippers =>
      if (zippers.length != which.n)
        F fail {
          val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
          failMsg(s"${q}uery failed: [${results.desc}]. Expected ${which.n} results, not ${zippers.length}.")
        }
      else
        F pass zippers(which.m - 1)
    }
  }

  private final def failMsg(msg: String): String =
    msg + "\n" + describe
}

object DomZipperBase {
  private val cssCondStart = "(^|, *)".r
  private def cssPrepend_>(a: String) = cssCondStart.replaceAllIn(a, "$1> ")

  trait Store[F[_], Dom, A, Self[G[_], B] <: Store[G, Dom, B, Self]] extends DomZipper[F, Dom, A, Self] {
    protected type Pos
    protected def pos: Pos
    protected def peek: Peek[A]
    protected def newStore[B](pos: Pos, peek: Peek[B]): Self[F, B]

    protected final type Peek[B] = Pos => B

    protected final def newStore[B](peek: Peek[B]): Self[F, B] =
      newStore(pos, peek)

    override final def map[B](f: A => B): Self[F, B] =
      newStore(f compose peek)

    override final def duplicate: Self[F, Self[F, A]] =
      newStore(newStore(_, peek))

    override final def extend[B](f: Self[F, A] => B): Self[F, B] =
      duplicate.map(f)

    override final def extract: A =
      peek(pos)
  }

  trait WithStore[F[_], Dom, A, Self[G[_], B] <: WithStore[G, Dom, B, Self]] extends DomZipperBase[F, Dom, A, Self]
      with Store[F, Dom, A, Self] {
    override final protected type Pos = Layers[Dom]
    override final protected def pos = layers
    override final protected[domzipper] def addLayer(n: Layer[Dom]) = newStore(layers add n, peek)
  }

  final case class Layers[Dom](init: Vector[Layer[Dom]], last: Layer[Dom]) {
    def all = init :+ last
    @inline def latest = last
    def add(l: Layer[Dom]) = Layers(all, l)
  }
  object Layers {
    def init[Dom](l: Layer[Dom]) = Layers(Vector.empty, l)
  }
}