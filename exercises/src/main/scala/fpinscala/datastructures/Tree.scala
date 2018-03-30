package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((a, b) => 1 + a + b)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(v => v)((a, b) => a max b)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((a, b) => 1 + (a max b))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

  

}

object MainTree {

  def main(args: Array[String]): Unit = {

    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val leaf3 = Leaf(3)
    val leaf4 = Leaf(4)

    val branch1 = Branch(leaf1, leaf2)
    val branch2 = Branch(leaf3, leaf4)
    val tree = Branch(branch1, branch2)

    /** Ex 3.25 */
    println("size result: " + Tree.size(tree)) //7
    /** Ex 3.26 */
    println("maximum result: " + Tree.maximum(tree)) //4
    /** Ex 3.27 */
    println("depth result: " + Tree.depth(tree)) //2
    /** Ex 3.28 */
    println("map result: " + Tree.map(tree)(_ + 1)) //(2,3,4,5)
    /** Ex 3.29 */
    println("sizeViaFold result: " + Tree.sizeViaFold(tree)) //7
    println("maximumViaFold result: " + Tree.maximumViaFold(tree)) //4
    println("depthViaFold result: " + Tree.depthViaFold(tree)) //2
    println("mapViaFold result: " + Tree.mapViaFold(tree)(_ + 2)) //(3,4,5,6)

  }
}

