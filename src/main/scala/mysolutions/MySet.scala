package mysolutions

import exercises.MySet

trait MySet[A] extends (A => Boolean) {

  /*
    EXERCISE - implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A] // union

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
 EXERCISE #2
 - removing an element
 - intersection with another set
 - difference with another set
*/
  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A] // difference
  def &(anotherSet: MySet[A]): MySet[A]  // intersection


  // EXERCISE #3 - implement a unary_! = NEGATION of a set
  // set[1,2,3] =>
  def unary_! : MySet[A] = new InvertedSet[A](this)
}

class PropertyBasedSet[A](prop: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = prop(elem)
  override def +(elem: A): MySet[A] = new PropertyBasedSet(_ == elem || prop(elem))
  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet(a => anotherSet(a) || contains(a))
  override def map[B](f: A => B): MySet[B] = ??? // TODO new PropertyBasesSet(b => b == f(a))
  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???
  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet(a => predicate(a) && contains(a))
  override def foreach(f: A => Unit): Unit = ???
  override def -(elem: A): MySet[A] = new PropertyBasedSet(a => elem != a && prop(elem))
  override def --(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet(a => !anotherSet.contains(a) && contains(a))
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
}


class InvertedSet[A] (base: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = !base.contains(elem)
  override def +(elem: A): MySet[A] = invert(base.-(elem))
  override def ++(anotherSet: MySet[A]): MySet[A] = invert(base.--(anotherSet))
  override def map[B](f: A => B): MySet[B] = invert(base.map(f))
  override def flatMap[B](f: A => MySet[B]): MySet[B] = invert(base.flatMap(f))
  override def filter(predicate: A => Boolean): MySet[A] = invert(base.filter(x => !predicate(x)))
  override def foreach(f: A => Unit): Unit = ??? // Will not terminate
  override def -(elem: A): MySet[A] = invert(base.+(elem))
  override def --(anotherSet: MySet[A]): MySet[A] = invert(base.++(anotherSet))
  override def &(anotherSet: MySet[A]): MySet[A] = invert(filter(anotherSet))

  private def invert[T](b: MySet[T]): MySet[T] = new InvertedSet(b)
}


class NonEmpty[A](
  head: A,
  tail: MySet[A],
) extends MySet[A] {
  override def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if (contains(elem)) this
    else new NonEmpty(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = (anotherSet + head) ++ tail
  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)
  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)
  override def filter(predicate: A => Boolean): MySet[A] =
    if (predicate(head))  tail.filter(predicate) + head
    else tail.filter(predicate)

  override def -(elem: A): MySet[A] = filter(_ != elem)
  override def --(anotherSet: MySet[A]): MySet[A] = filter(e => !anotherSet(e))
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
}

class Empty[A] () extends MySet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new NonEmpty(elem, this)
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  override def map[B](f: A => B): MySet[B] = new Empty[B]
  override def flatMap[B](f: A => MySet[B]): MySet[B] = new Empty[B]
  override def filter(predicate: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()
  override def -(elem: A): MySet[A] = this
  override def --(anotherSet: MySet[A]): MySet[A] = this
  override def &(anotherSet: MySet[A]): MySet[A] = this

}

object Test extends App {
  System.out.println("Start")
  System.out.println(new NonEmpty(1, new NonEmpty(3, new Empty[Int])).contains(1))
  System.out.println(new NonEmpty(1, new NonEmpty(3, new Empty[Int])).contains(2))
  System.out.println(!(new NonEmpty(1, new NonEmpty(3, new Empty[Int]))).contains(2))

}