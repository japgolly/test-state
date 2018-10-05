package teststate.domzipper

import DomZipper._
import DomZipperBase._
import ErrorHandler.Id

trait DomZipperBase[F[_], A, Self[G[_]] <: DomZipper[G, A, Self]] extends DomZipper[F, A, Self] {
  import DomCollection.Container

  private final def allLayers =
    prevLayers :+ curLayer

  final override def describe: String =
    s"DESC: ${allLayers.iterator.map(_.display) mkString " -> "}\nHTML: $outerHTML"

  // ==================
  // Self configuration
  // ==================

  protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]): Self[G]

  protected implicit val $: CssSelEngine[A, A]

  protected val prevLayers: Vector[Layer[A]]
  protected val curLayer: Layer[A]
  protected[domzipper] def addLayer(nextLayer: Layer[A]): Self[F]

  final override def scrubHtml(f: HtmlScrub): Self[F] =
    copySelf(htmlScrub >> f, F)

  final def failBy[G[_]](g: ErrorHandler[G]): Self[G] =
    copySelf(htmlScrub, g)

  final def failToOption: Self[Option           ] = failBy(ErrorHandler.ReturnOption)
  final def failToEither: Self[Either[String, ?]] = failBy(ErrorHandler.ReturnEither)
  final def throwErrors : Self[Id               ] = failBy(ErrorHandler.Throw)

  // ====================
  // DOM & DOM inspection
  // ====================

  protected def _outerHTML: String
  protected def _innerHTML: String
  protected def collect[C[_]](sel: String, C: Container[F, C]): DomCollection[Self, F, C, A]
  protected def collectChildren[C[_]](desc: String, C: Container[F, C]): DomCollection[Self, F, C, A]
  protected def collectChildren[C[_]](desc: String, sel: String, C: Container[F, C]): DomCollection[Self, F, C, A]

  final override def dom: A = curLayer.dom

  final override def collect01(sel: String): DomCollection[Self, F, Option, A] = collect(sel, F.C01)
  final override def collect0n(sel: String): DomCollection[Self, F, Vector, A] = collect(sel, F.C0N)
  final override def collect1n(sel: String): DomCollection[Self, F, Vector, A] = collect(sel, F.C1N)

  final override def children01: DomCollection[Self, F, Option, A] = collectChildren(">*", F.C01)
  final override def children0n: DomCollection[Self, F, Vector, A] = collectChildren(">*", F.C0N)
  final override def children1n: DomCollection[Self, F, Vector, A] = collectChildren(">*", F.C1N)

  final override def children01(sel: String): DomCollection[Self, F, Option, A] = collectChildren(cssPrepend_>(sel), sel, F.C01)
  final override def children0n(sel: String): DomCollection[Self, F, Vector, A] = collectChildren(cssPrepend_>(sel), sel, F.C0N)
  final override def children1n(sel: String): DomCollection[Self, F, Vector, A] = collectChildren(cssPrepend_>(sel), sel, F.C1N)

  // =======
  // Descent
  // =======

  protected def _parent: F[A]

  final lazy val parent: F[Self[F]] =
    F.map(_parent)(a => addLayer(Layer("parent", ":parent", a)))

  final def runCssQuery(sel: String): CssSelResult[A] =
    $.run(sel, curLayer.dom)

  final override def apply(name: String, sel: String, which: MofN): F[Self[F]] = {
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

  final override def child(name: String, sel: String, which: MofN): F[Self[F]] = {
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
}