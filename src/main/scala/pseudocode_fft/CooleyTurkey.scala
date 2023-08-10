package pseudocode_fft

import pseudocode_fft.BitReversalPermutation.bitReversalPermutation
import spire.implicits.*
import spire.math.Complex
import spire.math.Complex.rootOfUnity

import java.lang.Integer.bitCount


object CooleyTurkey:

  def fft( u: Array[Complex[Double]] ): Array[Complex[Double]] =

    val n = u.length; require(bitCount(n) == 1)
    val v = bitReversalPermutation(u)

    def fft( off: Int, n: Int ): Unit =

      if n <= 1 then return;

      val half = n / 2
      val  mid = off + half

      fft(off,half)
      fft(mid,half)

      for i <- 0 until half do
        val x = v(off + i)
        val y = v(mid + i) * Complex.rootOfUnity[Double](n,-i)
        v(off + i) = x+y
        v(mid + i) = x-y
      end for

    end fft

    fft(0,n)
    return v

  end fft


  def ifft( u: Array[Complex[Double]] ): Array[Complex[Double]] =

    val n = u.length; require(bitCount(n) == 1)
    val v = bitReversalPermutation(u)

    def ifft( off: Int, n: Int ): Unit =

      if n <= 1 then return;

      val half = n / 2
      val  mid = off + half

      ifft(off,half)
      ifft(mid,half)

      for i <- 0 until half do
        val x = v(off + i)
        val y = v(mid + i) * Complex.rootOfUnity[Double](n,i)
        v(off + i) = x + y
        v(mid + i) = x - y
      end for

    end ifft

    ifft(0,n)

    for i <- v.indices do v(i) /= n

    return v

  end ifft

end CooleyTurkey
