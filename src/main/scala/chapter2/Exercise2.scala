package chapter2

object Exercise2 {
  def main (args: Array[String] ): Unit = {
    val numbers = Array(1,2,3)
    println("Sorted numbers ascending should be true and is " + isSorted(numbers, ascendingNumbers))
    println("Sorted numbers ascending should be true and is " + isSorted(Array(1), ascendingNumbers))
    println("Sorted numbers ascending should be true and is " + isSorted2(Array(1), ascendingNumbers))
    println("Sorted numbers descending should be false and is " + isSorted(numbers.reverse, ascendingNumbers))
    println("UnSorted numbers should be false and is " + isSorted(Array(2,5,1,4,7), ascendingNumbers))

    println("Sorted numbers descending should be true and is " + isSorted(Array(4,3,2,1), (x:Int, y:Int) => x > y))
  }

  def ascendingNumbers(x:Int, y:Int): Boolean = x < y

  /**
    * My first solution, not using the go function and not using suggestions of the book.
    * @param as the array
    * @param ordered compare function
    * @tparam A Type, can be any
    * @return true is array is sorted
    */
  def isSorted[A](as:Array[A], ordered:(A,A) => Boolean) : Boolean = {
    @annotation.tailrec
    def compareArray(head:A, newHead:A, tail:Array[A], ordered:(A,A) => Boolean) : Boolean =
      if (!ordered(head, newHead)) false
      else
        if (tail.isEmpty) true
        else compareArray(newHead, tail.head, tail.tail, ordered)


    if (as.isEmpty || as.length == 1) true
    else compareArray(as.head, as.tail.head, as.tail.tail, ordered)
  }

  /**
    * Solution using suggestions from the book
    * @param as the array to verify
    * @param gt ordering functiin
    * @tparam A type to use, can be any type
    * @return true is ordered.
    */
  def isSorted2[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(as: Array[A]): Boolean =
      if (as.isEmpty || as.length == 1) true
      else if (gt(as.head, as.tail.head)) false else go(as.tail)

    go(as)
  }

}
