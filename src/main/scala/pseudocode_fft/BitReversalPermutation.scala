package pseudocode_fft

import spire.math.Complex

import java.lang.Integer.{bitCount, numberOfLeadingZeros, reverse as bitReverse}


object BitReversalPermutation:

  def bitReversalPermutation( u: Array[Complex[Double]] ): Array[Complex[Double]] =
    val n = u.length; require( bitCount(n) == 1 )
    val s = 1 + numberOfLeadingZeros(n)

    return Array.tabulate(n)( i => u(bitReverse(i<<s)) )
  end bitReversalPermutation

end BitReversalPermutation
