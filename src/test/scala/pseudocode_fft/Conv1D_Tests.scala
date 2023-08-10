package pseudocode_fft

import org.scalacheck.{Gen, Prop}
import pseudocode_fft.FloatUtils.allClose
import spire.implicits.*
import spire.math.Complex
import utest.*
import pseudocode_fft.Conv1D.pwc

import scala.collection.immutable.ArraySeq.unsafeWrapArray


object Conv1D_Tests extends TestSuite:

  override def tests: Tests = Tests{

    val propChecker = PropChecker.withParams(minSuccessfulTests = 10_000)

    def genReal = Gen.chooseNum(-100.0, +100.0)

    def genComplex = for
      re <- genReal
      im <- genReal
    yield Complex(re, im)

    def genUV = for
      e <- Gen.choose(0,12)
      genUV = Gen.containerOfN[Array, Complex[Double]](1 << e, genComplex)
      u <- genUV
      v <- genUV
    yield (u,v)

    test("pwc") {

      propChecker check Prop.forAll(genUV){
        (u,v) =>
          val n = u.length
          require(v.length == n)

          val a = u.clone
          val b = v.clone
          val tst = unsafeWrapArray( pwc(a,b) )

          assert{
            val x = unsafeWrapArray(a)
            val y = unsafeWrapArray(u)
            x == y
          }

          assert {
            val x = unsafeWrapArray(b)
            val y = unsafeWrapArray(v)
            x == y
          }

          val ref =
            val c = Array.fill(u.length)(Complex.zero[Double])

            for( i <- 0 until n )
            for( j <- 0 until n ) {
              var k = i+j
              if  k >= n then k -= n
              c(k) += a(i) * b(j)
            }

            unsafeWrapArray(c)
          end ref

          assert( allClose(tst,ref) )
          true
      }

    }

  }

end Conv1D_Tests
