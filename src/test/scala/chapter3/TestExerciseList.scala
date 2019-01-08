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

  test("should set new head") {
    assert(setHead(List(1,2,3), 4) == List(4,2,3) )
  }

  test("should set new head for single element") {
    assert(setHead(List(1), 4) == List(4) )
  }
  test("should set new head for empty list") {
    assert(setHead(List(), 4) == List(4) )
  }

  test("should return first three items") {
    assert(init(List(1,2,3,4)) == List(1,2,3))
  }

  test("should return nil as List is now empty") {
    assert(init(List(1)) == Nil)
  }
  test("lenght of list should be number of arguments") {
    assert(length(List(1,2,2,2)) == 4)
  }


  test("revers of list ") {
    assert(reverse(List(1,2,3,4)) == List(4,3,2,1))
  }
  test("revers of list using foldLeft") {
    assert(reverseFoldLeft(List(1,2,3,4)) == List(4,3,2,1))
  }

  test("append one list to another") {
    assert(appendViaFoldLeft(List(1,2,3,4), List(5,6,7,8)) == List(4,3,2,1,5,6,7,8))
  }
  test("append one list to another with foldRight") {
    assert(appendViaFoldRight(List(1,2,3,4), List(5,6,7,8)) == List(1,2,3,4,5,6,7,8))
  }

}
