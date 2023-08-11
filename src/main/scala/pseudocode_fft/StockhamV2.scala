package pseudocode_fft

import spire.implicits.*
import spire.math.Complex
import spire.math.Complex.rootOfUnity

import java.lang.Integer.bitCount
import java.lang.System.arraycopy


/**
 * A variant of the StockhamFFT that applies the bit-reversal permutation
 * during recursion instead of performing it at the very end like [[Stockham]].
 */
object StockhamV2:

  def fft( u: Array[Complex[Double]] ): Array[Complex[Double]] =

    // Implementation Details
    // ======================
    // Lets say we have a polynomial P(x) with p as its coefficient vector:
    //
    //   P(x) = p[0] + p[1]*x + p[2]*x² + ... + p[n-1] * x**(n-1)
    //
    // Then the FFT of p can be interpreted as evaluating P(x) at integer powers of the root of unity:
    //
    //   fft(p) = [P(1), P(w[n]), P(w[n]²), P(w[n]³), ..., P(w[n]**n)]
    //
    // Where w[n] is the principal n-th root of unit. In order to divide and conquer the problem,
    // we now want to find a way to split this evaluation into two smaller problem. To do that,
    // let's look at the even indexed and odd indexed evaluations separately.
    //
    // Even Indices
    // ------------
    // Let's first look at the even indices:
    //
    //   P(w[n]**(2*k)) = p[0]
    //                  + p[1] * w[n]**(2*k)
    //                  + p[2] * w[n]**(2*k*2)
    //                  + p[3] * w[n]**(2*k*3)
    //                  + ...
    //                  + p[n/2]   * w[n]**( 2*k*(n/2) )
    //                  + p[n/2+1] * w[n]**( 2*k*(n/2 + 1) )
    //                  + p[n/2+2] * w[n]**( 2*k*(n/2 + 2) )
    //                  + p[n/2+3] * w[n]**( 2*k*(n/2 + 3) )
    //                  + ...
    //
    //                  = p[0]
    //                  + p[1] * w[n]**(2*k)
    //                  + p[2] * w[n]**(2*k*2)
    //                  + p[3] * w[n]**(2*k*3)
    //                  + ...
    //                  + (w[n]**n)**k * (
    //                      p[n/2]
    //                    + p[n/2+1] * w[n]**(2*k)
    //                    + p[n/2+2] * w[n]**(2*k*2)
    //                    + p[n/2+3] * w[n]**(2*k*3)
    //                    + ...
    //                  )
    //
    //                  = (p[0] + p[n/2])
    //                  + (p[1] + p[n/2+1]) * w[n]**(2*k)
    //                  + (p[2] + p[n/2+2]) * w[n]**(2*k*2)
    //                  + (p[3] + p[n/2+3]) * w[n]**(2*k*3)
    //                  + ...
    //                  + (p[n/2-1] + p[n-1]) * w[n]**(2*k*(n/2-1))
    //
    // Keep in mind that:
    //
    //   (w[n]**n)**k  = 1
    //    w[n]**(2*k)  = (w[n]**2)**k = w[n/2]**k
    //
    // So evaluating the even polynomial at the even indices can be done by a half-sized FFT:
    //
    //   fft([p[0]+p[n/2], p[1]+p[n/2+1], ...]) = [P(w[n]**0), P(w[n]**2), P(w[n]**4), ..., P(w[n]**(n-2))]
    //
    //
    // Odd Indices
    // -----------
    // Okay so now lets look at the odd power terms:
    //
    //   P(w[n]**(2*k+1)) = p[0]
    //                    + p[1] * w[n]**(2*k   + 1)
    //                    + p[2] * w[n]**(2*k*2 + 2)
    //                    + p[3] * w[n]**(2*k*3 + 3)
    //                    + ...
    //                    + p[n/2]   * w[n]**(2*k*(n/2)     + (n/2)    )
    //                    + p[n/2+1] * w[n]**(2*k*(n/2 + 1) + (n/2 + 1))
    //                    + p[n/2+2] * w[n]**(2*k*(n/2 + 2) + (n/2 + 2))
    //                    + p[n/2+3] * w[n]**(2*k*(n/2 + 3) + (n/2 + 3))
    //                    + ...
    //
    //                    = p[0]
    //                    + p[1] * w[n]**(2*k   + 1)
    //                    + p[2] * w[n]**(2*k*2 + 2)
    //                    + p[3] * w[n]**(2*k*3 + 3)
    //                    + ...
    //                    + p[n/2]   * w[n]**(2*k*(n/2)     + (n/2)    )
    //                    + p[n/2+1] * w[n]**(2*k*(n/2 + 1) + (n/2 + 1))
    //                    + p[n/2+2] * w[n]**(2*k*(n/2 + 2) + (n/2 + 2))
    //                    + p[n/2+3] * w[n]**(2*k*(n/2 + 3) + (n/2 + 3))
    //                    + ...
    //
    //                    = p[0]
    //                    + p[1] * w[n]**(2*k   + 1)
    //                    + p[2] * w[n]**(2*k*2 + 2)
    //                    + p[3] * w[n]**(2*k*3 + 3)
    //                    + ...
    //                    + (w[n]**n)**k * w[n]**(n/2) * (
    //                        p[n/2]
    //                      + p[n/2+1] * w[n]**(2*k)   * w[n]
    //                      + p[n/2+2] * w[n]**(2*k*2) * w[n]**2
    //                      + p[n/2+3] * w[n]**(2*k*3) * w[n]**3
    //                      + ...
    //                    )
    //
    //                    =  p[0] - p[n/2]
    //                    + (p[1] - p[n/2+1]) * w[n]    * w[n]**(2*k)
    //                    + (p[2] - p[n/2+2]) * w[n]**2 * w[n]**(2*k*2)
    //                    + (p[3] - p[n/2+3]) * w[n]**3 * w[n]**(2*k*3)
    //                    + ...
    //                    + (p[n/2-1] - p[n-1]) * w[n]**(n/2 - 1) * w[n]**(2*k*(n/2-1))
    //
    // Keep in mind that:
    //
    //   w[n]**(n/2) = -1
    //
    // So just like with the even indices, we can use a half-sized FFT to compute the odd-index
    // of the full-size FFT:
    //
    //   fft([
    //      p[0] - p[n/2],
    //     (p[1] - p[n/2+1]) * w[n],
    //     (p[2] - p[n/2+2]) * w[n]**2,
    //     (p[3] - p[n/2+3]) * w[n]**3,
    //     ...,
    //     (p[n/2-1] - p[n-1]) * w[n]**(n/2 - 1)
    //   ]) = [P(w[n]**1), P(w[n]**3), P(w[n]**5), ..., P(w[n]**(n-1))]

    val n = u.length; require(bitCount(n) == 1)
    val v = u.clone
    val w = new Array[Complex[Double]](n)

    def fft( off: Int, n: Int ): Unit =

      if n <= 1 then return;

      val half = n / 2
      val  mid = off + half

      for i <- 0 until half do
        val x = v(off + i)
        val y = v(mid + i)
        v(off + i) =  x+y
        v(mid + i) = (x-y) * Complex.rootOfUnity[Double](n,-i)
      end for

      fft(off,half)
      fft(mid,half)

      for i <- 0 until half do
        w(off + 2*i  ) = v(off+i)
        w(off + 2*i+1) = v(mid+i)
      end for
      arraycopy(w,off, v,off, n)

    end fft

    fft(0,n)
    return v

  end fft


  def ifft( u: Array[Complex[Double]] ): Array[Complex[Double]] =

    val n = u.length; require(bitCount(n) == 1)
    val v = u.clone
    val w = new Array[Complex[Double]](n)

    def ifft( off: Int, n: Int ): Unit =

      if n <= 1 then return;

      val half = n / 2
      val  mid = off + half

      for i <- 0 until half do
        val x = v(off + i)
        val y = v(mid + i)
        v(off + i) = x+y
        v(mid + i) =(x-y) * Complex.rootOfUnity[Double](n,i)
      end for

      ifft(off, half)
      ifft(mid, half)

      for i <- 0 until half do
        w(off + 2 * i) = v(off + i)
        w(off + 2 * i + 1) = v(mid + i)
      end for
      arraycopy(w, off, v, off, n)

    end ifft

    ifft(0,n)

    for i <- v.indices do v(i) /= n

    return v

  end ifft

end StockhamV2
