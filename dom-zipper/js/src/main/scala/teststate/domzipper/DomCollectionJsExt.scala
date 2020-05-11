package teststate.domzipper

import org.scalajs.dom.html
import scala.reflect.ClassTag
import teststate.domzipper.DomZipperJsF.{Dom, DomCollection, safeCastDom}

object DomCollectionJsExt {
  trait Exports {
    final implicit def toDomCollectionJsExt[F[_], C[_], A](a: DomCollection[F, C, A]): DomCollectionJsExt[F, C, A] =
      new DomCollectionJsExt(a)
  }
}

final class DomCollectionJsExt[F[_], C[_], A](private val self: DomCollection[F, C, A]) extends AnyVal {
  def domsAs[D <: Dom: ClassTag]: F[C[D]] = {
    implicit val F = self.F
    self.traverseDoms(safeCastDom[F, D])
  }

  def domsAsHtml: F[C[html.Element]] =
    domsAs[html.Element]

  def domsAsButton: F[C[html.Button]] =
    domsAs[html.Button]

  def domsAsInput: F[C[html.Input]] =
    domsAs[html.Input]

  def domsAsTextArea: F[C[html.TextArea]] =
    domsAs[html.TextArea]
}