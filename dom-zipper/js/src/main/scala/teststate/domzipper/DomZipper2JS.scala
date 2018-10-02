package teststate.domzipper

import Simplify._
import org.scalajs.dom
import org.scalajs.dom.html
import JsDomExt._
import ErrorHandler.ErrorHandlerOptionOps
import ErrorHandler.ErrorHandlerResultOps
import scala.reflect.ClassTag
import scala.scalajs.js

object X {

  type Dom = dom.Element

  private def liftNode[F[_]](n: dom.Node)(implicit F: ErrorHandler[F]): F[Dom] =
    n match {
      case e: dom.Element => F pass e
      case x              => F fail s"Not an element: $x"
    }

  final class DomZipperJS[F[_]](override protected val prevLayers: Vector[Layer[Dom]],
                                override protected val curLayer: Layer[Dom]
                               )(implicit
                                 override protected val $: CssSelEngine[Dom, Dom],
                                 override protected val htmlScrub: HtmlScrub,
                                 override protected val F: ErrorHandler[F]
                               ) extends DomZipper2[F, Dom] {

    override type Self[G[_]] = DomZipperJS[G]

    override protected def self = this

    override protected def copySelf[G[_]](h: HtmlScrub, g: ErrorHandler[G]) =
      new DomZipperJS(prevLayers, curLayer)($, h, g)

    override protected[domzipper] def addLayer(nextLayer: Layer[Dom]) =
      new DomZipperJS(prevLayers :+ curLayer, nextLayer)

    override protected def _parent: F[Dom] =
      liftNode(dom.parentNode)

    override protected def _outerHTML: String =
      dynamicString(_.outerHTML)

    override protected def _innerHTML: String =
      dynamicString(_.innerHTML)

    private def newDomCollection[C[_]](desc: String, result: CssSelResult[Dom], C: DomCollection.Container[F, C]): Collection[C] =
      new DomCollection[Self, F, C, Dom](this, _.addLayer(_), desc, result, None, C)

    override protected def collect[C[_]](sel: String, C: DomCollection.Container[F, C]): Collection[C] =
      newDomCollection(sel, runCssQuery(sel), C)

    private def childIterator: Iterator[Dom] =
      dom.childNodes.iterator.collect {
        case e: org.scalajs.dom.Element => e
      }

    override protected def collectChildren[C[_]](desc: String, C: DomCollection.Container[F, C]): Collection[C] =
      newDomCollection(desc, childIterator.toVector, C)

    override protected def collectChildren[C[_]](desc: String, sel: String, C: DomCollection.Container[F, C]): Collection[C] = {
      val all = runCssQuery(sel).toSet
      newDomCollection(desc, childIterator.filter(all.contains).toVector, C)
    }

    override def matches(css: String): F[Boolean] =
      F pass dom.matches(css)

    override def getAttribute(name: String): Option[String] =
      Option(dom.attributes.getNamedItem(name)).map(_.value)

    override def tagName: String =
      dom.tagName

    override def innerText: String =
      dom.textContent.trim

    override def checked: F[Boolean] =
      dynamicMethod[Boolean](_.checked) orFail s".checked failed on $dom."

    override def classes: Set[String] =
      dom match {
        case h: html.Element =>
          val c = h.classList
          (0 until c.length).map(c.apply)(collection.breakOut)
        case _ =>
          Set.empty
      }

    override def value: F[String] =
      dynamicMethod[String](_.value.toString) orFail s".value failed on $dom."

    /** Cast DOM to [[js.Dynamic]] and invoke a method expected to return `A` if successful. */
    def dynamicMethod[A](f: js.Dynamic => Any): Option[A] =
      f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[A]].toOption

    /** Cast DOM to [[js.Dynamic]], invoke a method, return the result as a `String`. */
    def dynamicString(f: js.Dynamic => Any): String =
      dynamicMethod[Any](f).fold("undefined")(_.toString)

    /*
    def as[NewCur <: Base](implicit ct: ClassTag[NewCur]): Out[DomZipper[NewCur, Next, Out]] =
      domAs[NewCur].map(d =>
        new DomZipper(prevLayers, curLayer.copy(dom = d), htmlScrub))

    def asHtml: Out[DomZipper[html.Element, html.Element, Out]] =
      as[html.Element].map(_.withHtmlChildren)

    def forceAs[NewCur <: Base]: Out[DomZipper[NewCur, Next, Out]] =
      this.asInstanceOf[Out[DomZipper[NewCur, Next, Out]]]

    def forceChildren[A <: NextBase]: DomZipper[Cur, A, Out] =
      new DomZipper(prevLayers, curLayer, htmlScrub)

    def widenChildren[A >: Next <: NextBase]: DomZipper[Cur, A, Out] =
      forceChildren

    def withHtmlChildren: DomZipper[Cur, html.Element, Out] =
      forceChildren
    */

    def domAs[D <: Dom](implicit ct: ClassTag[D]): F[D] =
      ct.unapply(dom) orFail s"${dom.nodeName} is not a ${ct.runtimeClass}."

    def domAsHtml: F[html.Element] =
      domAs[html.Element]

    def forceDomAs[D <: Dom]: D =
      dom.asInstanceOf[D]

    /** The currently selected option in a &lt;select&gt; dropdown. */
    def selectedOption: F[Option[html.Option]] =
      domAs[html.Select].map(s =>
        if (s.selectedIndex >= 0)
          Some(s.options(s.selectedIndex))
        else
          None
      )

    /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
    def selectedOptionText: F[Option[String]] =
      selectedOption.map(_.map(_.text))
  }

}

