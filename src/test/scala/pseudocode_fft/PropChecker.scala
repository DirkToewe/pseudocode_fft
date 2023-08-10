package pseudocode_fft

import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty.pretty
import org.scalacheck.{Prop, Test}


object PropChecker:

  private object UTestReporter extends Test.TestCallback:

    private val prettyParams = Pretty.defaultParams

    override def onTestResult( name: String, test: Test.Result ): Unit =
      inline def msg: String = pretty(test, prettyParams)

      test.status match
        case Test.PropException(_,err,_) => throw AssertionError(msg,err)
        case Test.Failed(_,_) => throw new AssertionError(msg)
        case _ => utest.assert(test.passed)
      end match
    end onTestResult

  end UTestReporter

  final class withParams( minSuccessfulTests: Int = 1_000_000 ):

    inline infix def check( prop: Prop ): Unit =
      prop `check` Test.Parameters.default
        .withTestCallback(UTestReporter)
        .withMinSuccessfulTests(minSuccessfulTests)
        .withLegacyShrinking(false)
        .withWorkers(8)
//        .withWorkers( getRuntime.availableProcessors - 4 max 1 )
    end check

  end withParams

end PropChecker
