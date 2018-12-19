package chapter2

object Exercise1  {
  def main(args: Array[String]): Unit = {


    println(fib(6))
    println(withoutAccumulatorFib(6))
  }

  /**
    * Starts from the front
    * 0 1 1 2 3 5 8
    *
    * 0 based here
    * @param n number of fibonacci to calculate
    * @return
    */
  def fib(n: Int): Int = {

    @annotation.tailrec
    def loop(n: Int, begin: Int, next: Int): Int =
      if (n == 0) begin
      else loop(n - 1, next, begin + next)


    loop(n, 0, 1)
  }

  /**
    * I would write it without accumulator if I did not follow the chapter.
    * It starts backwards
    * in my original we start with 0 an n=1 and 1st is 1, 2nd = 1, 3rd = 2, 4rd = 3, 5th = 5
    * @param n number for Fibonacci
    * @return the end value
    */
  def withoutAccumulatorFib(n: Int): Int =
    if (n <= 1) 0
    else if (n == 2) 1
    else withoutAccumulatorFib(n - 1) + withoutAccumulatorFib(n - 2)

}
