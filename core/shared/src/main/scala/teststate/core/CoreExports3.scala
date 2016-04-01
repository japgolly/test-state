package teststate.core

import acyclic.file

trait CoreExports3
  extends Transformer.ToOps
     with CoreComposition2.Implicits

object CoreExports3 extends CoreExports3
