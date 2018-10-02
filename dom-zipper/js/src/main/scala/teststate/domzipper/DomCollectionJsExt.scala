package teststate.domzipper

import scala.reflect.ClassTag
import teststate.domzipper.DomZipperJsF.{Dom, DomCollection, safeCastDom}

object DomCollectionJsExt {
  trait Exports {
    final implicit def toDomCollectionJsExt[F[_], C[_]](a: DomCollection[F, C]): DomCollectionJsExt[F, C] =
      new DomCollectionJsExt(a)
  }
}

final class DomCollectionJsExt[F[_], C[_]](private val self: DomCollection[F, C]) extends AnyVal {
  def domsAs[D <: Dom: ClassTag]: F[C[D]] = {
    implicit val F = self.F
    self.traverse(safeCastDom[F, D])
  }
}