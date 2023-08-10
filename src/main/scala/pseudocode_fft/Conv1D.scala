package pseudocode_fft

import spire.implicits.*
import spire.math.Complex

import java.lang.Integer.bitCount


object Conv1D:

  def pwc( u: Array[Complex[Double]], v: Array[Complex[Double]] ): Array[Complex[Double]] =

    val n = u.length
    require(v.length == n)
    require(bitCount(n) == 1)

    val a = u.clone; fft_fast(a,0,n)
    val b = v.clone; fft_fast(b,0,n)

    for i <- a.indices do a(i) = a(i) * b(i) / n

    ifft_fast(a,0,n)
    return a

  end pwc


  private def fft_fast( v: Array[Complex[Double]], off: Int, n: Int ): Unit =

    if n <= 1 then return

    // Stockham FFT but without bit-reversal permutation in the end
    val half = n / 2
    val  mid = off + half

    for i <- 0 until half do
      val x = v(off + i)
      val y = v(mid + i)
      v(off + i) = x + y
      v(mid + i) =(x - y) * Complex.rootOfUnity[Double](n,-i)
    end for

    fft_fast(v,off,half)
    fft_fast(v,mid,half)

  end fft_fast


  private def ifft_fast( v: Array[Complex[Double]], off: Int, n: Int ): Unit =

    if n <= 1 then return

    // Cooley-Turkey IFFT
    // but without bit-reversal permutation in the beginning
    // and without division by n in the end
    val half = n / 2
    val  mid = off + half

    ifft_fast(v,off,half)
    ifft_fast(v,mid,half)

    for i <- 0 until half do
      val x = v(off + i)
      val y = v(mid + i) * Complex.rootOfUnity[Double](n,i)
      v(off + i) = x + y
      v(mid + i) = x - y
    end for

  end ifft_fast

end Conv1D
