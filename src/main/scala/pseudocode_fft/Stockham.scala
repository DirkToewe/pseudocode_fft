package pseudocode_fft

import pseudocode_fft.BitReversalPermutation.bitReversalPermutation
import spire.implicits.*
import spire.math.Complex
import spire.math.Complex.rootOfUnity

import java.lang.Integer.bitCount


object Stockham:

  def fft( u: Array[Complex[Double]] ): Array[Complex[Double]] =

    val n = u.length; require(bitCount(n) == 1)
    val v = u.clone

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

    end fft

    fft(0,n)
    return bitReversalPermutation(v)

  end fft


  def ifft( u: Array[Complex[Double]] ): Array[Complex[Double]] =

    val n = u.length; require(bitCount(n) == 1)
    val v = u.clone

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

    end ifft

    ifft(0,n)

    for i <- v.indices do v(i) /= n

    return bitReversalPermutation(v)

  end ifft

end Stockham
