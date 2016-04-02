package teststate.domzipper

import org.scalajs.dom.{Element, html}
import Collector._
import DomZipper._
import ErrorHandler.ErrorHandlerResultOps

final class Collector[C[_], D <: Next, Next <: NextBase, Out[_]](from: DomZipper[_, Next, Out],
                                                                 sel: String,
                                                                 cont: Container[C, Out])
                                                                (implicit h: ErrorHandler[Out]){

  def isEmpty: Boolean =
    from.directSelect(sel).isEmpty

  def nonEmpty: Boolean =
    !isEmpty

  def as[DD <: D]: Collector[C, DD, Next, Out] =
    this.asInstanceOf[Collector[C, DD, Next, Out]]

  def asHtml(implicit ev: html.Element <:< D): Collector[C, html.Element, html.Element, Out] =
    this.asInstanceOf[Collector[C, html.Element, html.Element, Out]]

  def doms: Out[C[D]] = {
    val e1: Out[C[Element]] = cont(sel, from.directSelect(sel))
    val e2: Out[C[D]]       = e1.asInstanceOf[Out[C[D]]]
    e2
  }

  private def addLayer(d: D): DomZipper[D, Next, Out] =
    from.addLayer(Layer("collect", sel, d))

  def zippers: Out[C[DomZipper[D, Next, Out]]] =
    doms.map(cont.map(_)(addLayer))

  def mapDoms[A](f: D => A): Out[C[A]] =
    doms.map(cont.map(_)(f))

  def mapZippers[A](f: DomZipper[D, Next, Out] => A): Out[C[A]] =
    mapDoms(d => f(addLayer(d)))

  def outerHTMLs[A]: Out[C[String]] = mapZippers(_.outerHTML)
  def innerHTMLs[A]: Out[C[String]] = mapZippers(_.innerHTML)
  def innerTexts[A]: Out[C[String]] = mapZippers(_.innerText)
}

object Collector {

  trait Container[C[_], Out[_]] {
    def apply(sel: String, es: CssSelResult): Out[C[Element]]
    def map[A, B](c: C[A])(f: A => B): C[B]
  }

  final class Container01[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Option, Out] {
    override def apply(sel: String, es: CssSelResult) =
      es.length match {
        case 0 => h pass None
        case 1 => h pass Some(es.head)
        case n => h fail s"$n matches found for: $sel"
      }
    override def map[A, B](c: Option[A])(f: A => B) =
      c map f
  }

  final class Container0N[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Vector, Out] {
    override def apply(sel: String, es: CssSelResult) =
      h pass es.toVector
    override def map[A, B](c: Vector[A])(f: A => B) =
      c map f
  }

  final class Container1N[Out[_]](implicit h: ErrorHandler[Out]) extends Container[Vector, Out] {
    override def apply(sel: String, es: CssSelResult) =
      if (es.isEmpty)
        h fail s"No matches found for: $sel"
      else
        h pass es.toVector
    override def map[A, B](c: Vector[A])(f: A => B) =
      c map f
  }
}