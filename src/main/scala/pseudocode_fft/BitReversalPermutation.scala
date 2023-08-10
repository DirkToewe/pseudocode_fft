package pseudocode_fft

import java.lang.Integer.{bitCount, numberOfLeadingZeros, reverse as bitReverse}


object BitReversalPermutation:

  def bitReversalPermutation[T]( u: Array[T] ): Array[T] =

    val n = u.length; require( bitCount(n) == 1 )
    val v = u.clone
    val s = 1 + numberOfLeadingZeros(n)

    for i <- 1 until n-1 do
      val j = bitReverse(i<<s)
      if i < j then // <- only swap(i,j) once
        val vi = v(i)
        v(i) = v(j)
        v(j) = vi
      end if
    end for

    return v

  end bitReversalPermutation

end BitReversalPermutation
