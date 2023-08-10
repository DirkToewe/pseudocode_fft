package pseudocode_fft

import spire.implicits.*
import spire.math.Complex

import scala.collection.IndexedSeq as ISeq


object FloatUtils:

  def allClose(
    x: ISeq[Complex[Double]],
    y: ISeq[Complex[Double]],
    rtol: Double = 1e-5,
    atol: Double = 1e-8
  ): Boolean =

    def isClose( x: Complex[Double], y: Complex[Double] ): Boolean =
      x == y || {
        val tol = atol + rtol * (x.abs max y.abs)
        tol.isFinite && (x - y).abs <= tol
      }
    end isClose

    x.length == y.length && x.indices.forall {
      i => isClose(x(i), y(i))
    }

  end allClose

end FloatUtils
