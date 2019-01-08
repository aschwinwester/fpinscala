package chapter3

import scala.annotation.tailrec


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
    *
    * @param list some list of items
    * @tparam A type to use
    * @return Option of a list as list might be empty.
    */
  def tailOption[A](list: List[A]): Option[List[A]] = list match {
    case Nil => Option.empty
    case Cons(_, xs) => Option.apply(xs)
  }

  /**
    * 3.2 tail with throwing and error.
    *
    * @param list Some list of items
    * @tparam A type
    * @return all but the first as new List
    */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new RuntimeException("Unable to create tail of empty list")
    case Cons(_, xs) => xs
  }

  /**
    * Exercise 3.3 setHead
    * Replace the first item in the list.
    * Use the pattern matching on list to recognize the head and tail.
    *
    * @param list a list
    * @param head new item which should be the head.
    * @tparam A Generic
    * @return
    */
  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(_, tail) => Cons(head, tail)
  }

  /**
    * Exercise 3.4 drop.
    * Recursive call drop. Check boundaries for List and n.
    *
    * @param list some list to check
    * @param n    number of items to drop
    * @tparam A type
    * @return new list or original list.
    */
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) list
    else list match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }

  /**
    * Exercise 3.5 dropWhile, this is my initial version.
    * See the dropWhilePatternGuard for a more beautiful solution.
    *
    * @param l some list
    * @param f function to use for validation
    * @tparam A type
    * @return new list or original list.
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else Cons(head, tail)
  }

  /**
    * Exercise 3.5 dropWhile using pattern guard.
    * Did not know Scala could do this.
    *
    * @param l some list
    * @param f functiom to use
    * @tparam A type
    * @return new list
    */
  def dropWhilePatternGuard[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(d, tail) if f(d) => dropWhilePatternGuard(tail, f)
    case _ => l
  }

  /**
    * Exercise 3.6 return list without last item.
    * @param l some list
    * @tparam A type if list
    * @return new list without last item
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil // already Nil, leave it for now.
    case Cons(_, Nil) => Nil // head with Nil should return Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  /**
    * This method is given in the book
    * @param as startling list
    * @param z starting value
    * @param f function
    * @tparam A Type of list
    * @tparam B type of z
    * @return result
    */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  def sum2(ns: List[Int]):Int = foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]):Double = foldRight(ns, 1.0)(_ * _)

  /**
    * Exercise 3.9, the operation is just starting value + 1.
    * @param as list
    * @tparam A type of list
    * @return Length as Int
    */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, x) => x + 1)

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, tail) => foldLeft(tail, f(z, h))(f)
  }

  /**
    * Exercise 3.11
    * @param ns list
    * @return sum of the list
    */
  def sum3(ns: List[Int]):Int = foldLeft(ns, 0)(_ + _)
  def product4(ns: List[Double]):Double = foldLeft(ns, 1.0)(_ * _)
  def length3[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  /**
    * Exercise 3.12 my own implementation without foldLeft.
    *
    * @param list to reverse
    * @tparam A type
    * @return list which is reversed
    */
  def reverse[A](list:List[A]):List[A] = {

    /**
      * The new head, should be placed before the heads in acc.
      * @param l a list
      * @param acc accumulater
      * @return new list
      */
    def accum(l:List[A], acc:List[A]):List[A] = l match {
      case Nil => acc
      case Cons(head, tail) => accum(tail, Cons(head, acc))
    }

    accum(list, List[A]())
  }

  /**
    * Exercise 3.12
    *  this answer looks in correct,
    * but head should be placed before already processed heads.
    * So Cons(h, acc) is correct.
    *
    * The acc is the list, append h as an element last
    * @param list a list
    * @tparam A Type
    * @return new list
    */
  def reverseFoldLeft[A](list:List[A]):List[A] = foldLeft(list, List[A]())( (acc, h) => Cons(h, acc))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2)( (acc, h) => Cons(h, acc))

  /**
    * see https://medium.com/@juntomioka/why-foldright-is-beautiful-7854ede3e133
    * @param l
    * @param r
    * @tparam A
    * @return
    */
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_,_))

}