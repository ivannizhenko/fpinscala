package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => List.empty
  }

  def toListTailRecursive: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n>1 => cons(h(),t().take(n-1))
    case Cons(h,_) if n==1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionFoldRight: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }

  //  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def onesUnfold(): Stream[Int] = unfold(1)(_ => Some(1,1))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def constantEfficient[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a,a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n+1))

  def fibs(): Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, go(n2, n1 + n2))
    }

    go(0, 1)
  }

  /*
   Scala provides shorter syntax when the first action of a function literal is to match on an expression.
   The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`,
   but we avoid having to choose a name for `p`, only to pattern match on it.
  */
  def fibsUnfold():Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  def find(start: Int, end: Int, arr: Array[Int]): Int = {
    if (start == end && arr(start) >= 515)
      return start
    val middle = (start + end) / 2
    if (arr(middle) < 515)
      find(middle + 1, end, arr)
    else
      find(start, middle, arr)
  }

}

object StreamMain {
  def main(args: Array[String]): Unit = {

    val stream = Stream.apply(1, 2, 3, 4, 5)

    println("Stream is: " + stream)
    println("Stream.toList is: " + stream.toList)
    println("Stream.toListTailRecursive is: " + stream.toListTailRecursive)
    println("Stream.take(3) is: " + stream.take(3).toList)
    println("Stream.takeWhile(_<5) is: " + stream.takeWhile(_ < 5).toList)
    println("Stream.takeWhileFoldRight(_<5) is: " + stream.takeWhileFoldRight(_ < 5).toList) // List(1,2,3,4)
    println("Stream.drop(3) is: " + stream.drop(3).toList)
    println("Stream.forAll(_<3) is: " + stream.forAll(_<3)) //false
    println("Stream.forAll(_<6) is: " + stream.forAll(_<6)) //true
    println("Stream.headOption is: " + stream.headOption)
    println("Stream.headOptionFoldRight is: " + stream.headOptionFoldRight)
    println("Stream.map(_*2) is: " + stream.map(_*2).toList) // (2,4,6,8,10)
    println("Stream.filter(_%2==0) is: " + stream.filter(_%2==0).toList) // (2,4)
    println("Stream.append(Stream.apply(6,7)) is: " + stream.append(Stream.apply(6,7)).toList) // (1,2,3,4,5,6,7)
    println("Stream.flatMap is: " + stream.flatMap(i => Stream.apply(i,i)).toList) // (1,1,2,2,3,3,4,4,5,5)
    println("Stream.ones.take(5) is: " + ones.take(5).toList) // (1,1,1,1,1)
    println("Stream.ones.exists(_%2!=0) is: " + ones.exists(_%2!=0)) // true
    println("Stream.ones.map(_+1).exists(_%2==0) is: " + ones.map(_+1).exists(_%2==0)) // true
//    println("Stream.ones.takeWhile(_==1) is: " + ones.takeWhile(_==1).toList) // StackOverflowError
    println("Stream.ones.forAll(_!=1) is: " + ones.forAll(_!=1)) // false
    println("Stream.onesUnfold.take(5) is: " + Stream.onesUnfold().take(5).toList) // (1,1,1,1,1)
    println("Stream.constant(5).take(3) is: " + Stream.constant(5).take(3).toList) // (5,5,5)
    println("Stream.constantEfficient(5).take(3) is: " + Stream.constantEfficient(5).take(3).toList) // (5,5,5)
    println("Stream.constantUnfold(5).take(3) is: " + Stream.constantUnfold(5).take(3).toList) // (5,5,5)
    println("Stream.from(5).take(3) is: " + Stream.from(5).take(3).toList) // (5,6,7)
    println("Stream.fromUnfold(5).take(3) is: " + Stream.fromUnfold(5).take(3).toList) // (5,6,7)
    println("Stream.fibs().take(7) is: " + Stream.fibs().take(7).toList) // (0,1,1,2,3,5,8)
    println("Stream.fibsUnfold().take(7) is: " + Stream.fibsUnfold().take(7).toList) // (0,1,1,2,3,5,8)


  }
}