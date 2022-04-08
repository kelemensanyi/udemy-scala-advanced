package mysolutions

import lectures.part2afp.LazyEvaluation.MyStream

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B]  // prepend operator
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concatenate two streams

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements out of this stream
  def takeAsList(n: Int): List[A]

  def toList: List[A]
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty = true
  def head = throw new Exception()
  def tail = throw new Exception()

  def #::[B](element: B): MyStream[B] = NonEmptyStream(element, this)
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit) = ()
  def map[B](f: Nothing => B) = this
  def flatMap[B](f: Nothing => MyStream[B]) = this
  def filter(predicate: Nothing => Boolean) = this

  def take(n: Int) = this
  def takeAsList(n: Int) = Nil
  def toList: List[Nothing] = Nil

}

class NonEmptyStream[+A](val head: A, tailParam: => MyStream[A]) extends MyStream[A] {
  def isEmpty = false

  lazy val tail = tailParam

  def #::[B >: A](element: B): MyStream[B] = NonEmptyStream(element, this)
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = NonEmptyStream(head, tail ++ anotherStream) // concatenate two streams

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def map[B](f: A => B): MyStream[B] = NonEmptyStream(f(head), tail.map(f))
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) NonEmptyStream(head, tail.filter(predicate))
    else tail.filter(predicate)

  def take(n: Int): MyStream[A] = if (n == 0) EmptyStream else NonEmptyStream(head, tail.take(n - 1)) // takes the first n elements out of this stream
  def takeAsList(n: Int): List[A] = if (n == 0) Nil else head :: tail.takeAsList(n - 1)

  def toList: List[A] = {
    @tailrec
    def go(acc: List[A], s: MyStream[A]): List[A] = {
      if (s.isEmpty) acc
      else go(s.head :: acc, s.tail)
    }
    go(Nil, this).reverse
  }

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = NonEmptyStream(start, from(generator(start))(generator))
}

object Main extends App {
  val naturals: MyStream[Int] = MyStream.from(0)(_ + 1)
  naturals.take(100).foreach(println)
  naturals.map(_ * 2).take(5).foreach(println)
  println(naturals.map(_ * 2).take(5).toList)

  val startFrom0 = 0 #:: naturals
  println(startFrom0.flatMap(x => new NonEmptyStream(x, new NonEmptyStream(x + 1, EmptyStream))).take(10).toList)

  val fibonacci = MyStream.from((1, 1)) {
    (a, b) => (b, a + b)
  }.map(_._1);
  System.out.println(fibonacci.take(10).toList)

  val primes = MyStream.from((2, naturals.tail.tail.tail)) {
    case (a, s) =>
      val filteredS = s.filter(_ % a != 0)
      (filteredS.head, filteredS)
  }.map(_._1)
  System.out.println(primes.take(30).toList)

}
