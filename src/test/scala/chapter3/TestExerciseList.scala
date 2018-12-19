package chapter3

import org.scalatest.FunSuite
import chapter3.List._

class TestExerciseList extends FunSuite {

  /**
    * the first case skips the 3 and is incorrect
    * the third is the correct one as the list is not empty.
    */
  test("should be x + y = 1 + 2") {

    val x = List(1,2,3,4,5) match {
      case Cons(z, Cons(2, Cons(4, _))) => z
      case Nil => 42
      case Cons(a, Cons(b, Cons(3, Cons(4, _)))) => a + b
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    assert(x == (1 + 2))

  }

  test("should return tail for non-empty list") {
    assert(tail(List(1,2,3)) == List(2,3) )
  }
}
