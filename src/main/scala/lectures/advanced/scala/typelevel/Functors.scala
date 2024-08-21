package lectures.advanced.scala.typelevel

import scala.util.{Success, Try}

object Functors extends App {

  val anIncrementedList = List(1, 2, 3).map(x => x + 1) // List(2, 3, 4)

  // Options
  val anOption: Option[Int] = Some(2)
  // Try
  val aTry: Try[Int] = Success(42)

  val aTransformedOption = anOption.map(x => x * 10)
  val aTransformedTry = aTry.map(x => x * 10)

  // Functors
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(aTry: Try[Int]): Try[Int] = aTry.map(_ * 10)

  trait Functor[C[_]] {
    def map[A, B](container: C[A])(f: A => B): C[B]
  }

  implicit object listFunctor extends Functor[List] {
    override def map[A, B](container: List[A])(f: A => B) = container.map(f)
  }

  // "stable" API
  def do10x[C[_]](container: C[Int])(implicit functor: Functor[C]): C[Int] =
    functor.map(container)(_ * 10)

  do10xList(List(1, 2, 3))
  do10x(List(1, 2, 3))

  trait Tree[+T]
  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] =
      Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](container: Tree[A])(f: A => B): Tree[B] =
      container match {
        case Leaf(value) => Leaf(f(value))
        case Branch(value, left, right) =>
          Branch(f(value), map(left)(f), map(right)(f))
      }
  }

  val tree =
    Tree.branch(1, Tree.branch(2, Tree.leaf(3), Tree.leaf(4)), Tree.leaf(5))

  println(do10x(tree))

}
