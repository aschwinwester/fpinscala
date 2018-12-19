package chapter2

object Exercise3 {

  def main(args: Array[String]): Unit = {

    val firstStep = curry(divide)
    val secondStep = firstStep(3.0f)
    val answer = secondStep(2)
    println(answer)
  }

  /**
    * Sample function used float to divide and returns nice text.
    * @param x float to use
    * @param y divide by int
    * @return text with float divided.
    */
  def divide(x:Float, y:Int):String = "done: " + x / y


  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)


  /**
    * We will use the example of partial 1 to create the answer
    * @param f a function which needs a and b
    * @tparam A Type a
    * @tparam B Type b
    * @tparam C Type c
    * @return partial function
    */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a :A) => (b:B) => f(a, b)

}
