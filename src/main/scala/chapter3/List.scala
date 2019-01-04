package chapter3


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * We can also return an option for tail on empty list.
    * @param list some list of items
    * @tparam A type to use
    * @return Option of a list as list might be empty.
    */
  def tailOption[A](list:List[A]): Option[List[A]] = list match {
    case Nil => Option.empty
    case Cons(_, xs) => Option.apply(xs)
  }

  /**
    * 3.2 tail with throwing and error.
    * @param list Some list of items
    * @tparam A type
    * @return all but the first as new List
    */
  def tail[A](list:List[A]): List[A] = list match {
    case Nil => throw new RuntimeException("Unable to create tail of empty list")
    case Cons(_, xs) => xs
  }

  /**
    * Exercise 3.3 setHead
    * Replace the first item in the list.
    * Use the pattern matching on list to recognize the head and tail.
    * @param list a list
    * @param head new item which should be the head.
    * @tparam A Generic
    * @return
    */
  def setHead[A](list:List[A], head:A): List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(_, tail) => Cons(head, tail)
  }

  /**
    * Exercise 3.4 drop.
    * Recursive call drop. Check boundaries for List and n.
    * @param list
    * @param n
    * @tparam A
    * @return
    */
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) list
    else list match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n -1)
    }
  }
}
