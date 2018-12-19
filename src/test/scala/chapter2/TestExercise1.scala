package chapter2
import org.scalatest.FunSuite
import chapter2.Exercise1.{
  fib, withoutAccumulatorFib
}

class TestExercise1 extends FunSuite {
  test("basic") {
    assert(fib(5) == 5)
  }

  test("zero based") {
    assert(fib(1) == 1)
  }

  test("without acc 1") {
    assert(withoutAccumulatorFib(1) == 0)
  }

  test("without acc fib 5") {
    assert(withoutAccumulatorFib(5) == 3)
  }

}
