package pseudocode_fft

import spire.math.Complex

import java.lang.Integer.{bitCount, numberOfLeadingZeros, reverse as bitReverse}


object BitReversalPermutation:

  def bitReversalPermutation( u: Array[Complex[Double]] ): Array[Complex[Double]] =
    require( bitCount(u.length) == 1 )
    val s = 1 + numberOfLeadingZeros(u.length)

    return Array.tabulate(u.length)( i => u(bitReverse(i<<s)) )
  end bitReversalPermutation

end BitReversalPermutation
