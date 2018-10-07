package teststate.domzipper

import DomZipper2._
import DomZipperBase2._
import ErrorHandler.Id

trait DomZipperBase2[F[_], A, Self[G[_], B] <: DomZipper2[G, B, Self]] extends DomZipper2[F, A, Self] {
  import DomCollection.Container

//  protected def extractFn: (Vector[Layer[Dom]], Layer[Dom]) => A
//
//  protected def withExtractFn[B]: ((Vector[Layer[Dom]], Layer[Dom]) => B) => Self[F, B]
//
//  override def map[B](f: A => B): Self[F, B] =
//    withExtractFn((x, y) => f(extractFn(x, y)))
//
//  override def extend[B](f: Self[F, A] => B): Self[F, B] =
//    duplicate.map(f)
//
//  override def duplicate: Self[F, Self[F, A]] =
//    new Self(prevLayers, curLayer, new Self[F, A](_, _, A))
//
//  override def extract: A =
//    extractFn(prevLayers, curLayer)

//  override def unfocus =
//    new Self(prevLayers, curLayer, (_, y) => y.dom)

  private final def allLayers =
    prevLayers :+ curLayer

  final override def describe: String =
    s"DESC: ${allLayers.iterator.map(_.display) mkString " -> "}\nHTML: $outerHTML"

  // ==================
  // Self configuration
  // ==================

  protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]): Self[G, A]

  protected implicit val $: CssSelEngine[Dom, Dom]

  protected val prevLayers: Vector[Layer[Dom]]
  protected val curLayer: Layer[Dom]
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
  protected def collect[C[_]](sel: String, C: Container[F, C]): DomCollection[Self, F, C, A]
  protected def collectChildren[C[_]](desc: String, C: Container[F, C]): DomCollection[Self, F, C, A]
  protected def collectChildren[C[_]](desc: String, sel: String, C: Container[F, C]): DomCollection[Self, F, C, A]

  final override def dom: Dom = curLayer.dom

  final override def collect01(sel: String): DomCollection[Self, F, Option, A] = collect(sel, F.XC01)
  final override def collect0n(sel: String): DomCollection[Self, F, Vector, A] = collect(sel, F.XC0N)
  final override def collect1n(sel: String): DomCollection[Self, F, Vector, A] = collect(sel, F.XC1N)

  final override def children01: DomCollection[Self, F, Option, A] = collectChildren(">*", F.XC01)
  final override def children0n: DomCollection[Self, F, Vector, A] = collectChildren(">*", F.XC0N)
  final override def children1n: DomCollection[Self, F, Vector, A] = collectChildren(">*", F.XC1N)

  final override def children01(sel: String): DomCollection[Self, F, Option, A] = collectChildren(cssPrepend_>(sel), sel, F.XC01)
  final override def children0n(sel: String): DomCollection[Self, F, Vector, A] = collectChildren(cssPrepend_>(sel), sel, F.XC0N)
  final override def children1n(sel: String): DomCollection[Self, F, Vector, A] = collectChildren(cssPrepend_>(sel), sel, F.XC1N)

  // =======
  // Descent
  // =======

  protected def _parent: F[Dom]

  final lazy val parent: F[Self[F, A]] =
    F.map(_parent)(dom => addLayer(Layer("parent", ":parent", dom)))

  final protected def runCssQuery(sel: String): CssSelResult[Dom] =
    $.run(sel, curLayer.dom)

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

object DomZipperBase2 {
  private val cssCondStart = "(^|, *)".r
  private def cssPrepend_>(a: String) = cssCondStart.replaceAllIn(a, "$1> ")

  trait Store[F[_], A, Self[G[_], B] <: Store[G, B, Self]] extends DomZipper2[F, A, Self] {
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

  trait WithStore[F[_], A, Self[G[_], B] <: WithStore[G, B, Self]] extends DomZipperBase2[F, A, Self]
      with Store[F, A, Self] {
    override final protected type Pos = (Vector[Layer[Dom]], Layer[Dom])
    override final protected def pos = (prevLayers, curLayer)
    override final protected[domzipper] def addLayer(n: Layer[Dom]) = newStore((prevLayers :+ curLayer, n), peek)
  }
}