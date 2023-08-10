package pseudocode_fft

import org.scalacheck.{Gen, Prop}
import pseudocode_fft.FloatUtils.allClose
import spire.algebra.Trig
import spire.implicits.*
import spire.math.Complex
import utest.*

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.ArraySeq.unsafeWrapArray


class FFT_TestTemplate(
   fft: Array[Complex[Double]] => Array[Complex[Double]],
  ifft: Array[Complex[Double]] => Array[Complex[Double]]
) extends TestSuite:

  override def tests: Tests = Tests{

    val propChecker = PropChecker.withParams(minSuccessfulTests = 10_000)

    def genReal = Gen.chooseNum(-100.0, +100.0)

    def genComplex = for
      re <- genReal
      im <- genReal
    yield Complex(re, im)

    def genU = for
      e <- Gen.choose(0, 10)
      v <- Gen.containerOfN[Array, Complex[Double]](1 << e, genComplex)
    yield v

    val i = Complex.i[Double]
    val trig = Trig[Complex[Double]]
    import trig.{exp, pi}


    test("fft"){

      propChecker check Prop.forAll(genU){
        u =>
          val v = u.clone
          val tst = unsafeWrapArray(fft(v))

          assert{
            // make sure v remains unchanged
            val a = unsafeWrapArray(u)
            val b = unsafeWrapArray(v)
            a == b
          }

          // https://en.wikipedia.org/wiki/Discrete_Fourier_transform
          val N = u.length

          val ref = ArraySeq.tabulate(N){ k =>
            Iterator.tabulate(N){ n =>
              u(n) * exp(-i*2*pi / N * k*n)
            }.reduce(_+_)
          }

          assert( allClose(tst,ref) )
          true
      }

    }


    test("ifft") {

      propChecker check Prop.forAll(genU) {
        u =>
          val v = u.clone
          val tst = unsafeWrapArray(ifft(v))

          assert {
            // make sure v remains unchanged
            val a = unsafeWrapArray(u)
            val b = unsafeWrapArray(v)
            a == b
          }

          // https://en.wikipedia.org/wiki/Discrete_Fourier_transform
          val N = u.length

          val ref = ArraySeq.tabulate(N) { k =>
            Iterator.tabulate(N) { n =>
              u(n) * exp(i * 2 * pi / N * k * n)
            }.reduce(_ + _) / N
          }

          assert(allClose(tst, ref))
          true
      }

    }

  }

end FFT_TestTemplate
