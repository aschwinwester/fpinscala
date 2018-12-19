package chapter2

object Exercise4 {

  def main(args: Array[String]): Unit = {

  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a:A, b:B) => f(a)(b)

}
