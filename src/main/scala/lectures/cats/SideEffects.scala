package lectures.cats

import lectures.cats.SideEffects.IO.{Done, FlatMap, More}

import java.util.concurrent.ConcurrentHashMap
import scala.annotation.tailrec
import scala.collection.mutable

object SideEffects extends App {
  val x: Int = {
    println("hello5")
    val s = Array(3, 2, 21)
    val adj = new ConcurrentHashMap[Int, List[Int]]()
    val bb = (x: Int) => List(3 + x)
    println(bb(3) + "+++++++++++++++++")
    5
  }

  // 1. Describe a computation
  // 2. Later, run it

  trait IO[+A] {
    import IO._
    def resume: Either[() => IO[A], A] =
      this match {
        case Done(a: A)  => Right(a)
        case More(thunk) => thunk().resume
        case FlatMap(t, f) =>
          t match {
            case Done(a2)     => f(a2).resume
            case More(thunk2) => Left(() => FlatMap(thunk2(), f))
            case FlatMap(t2, f2) =>
              FlatMap(t2, (x: Any) => FlatMap(f2(x), f)).resume
          }
      }

    @tailrec
    final def run: A = resume match {
      case Right(a)    => a
      case Left(thunk) => thunk().run
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): IO[List[B]] =
      as match {
        case Nil => Done(Nil)
        case (h :: t) =>
          More { () =>
            FlatMap(flatMap(t)(f), (lb: List[B]) => Done(f(h) ::: lb))
          }
      }
  }
  object IO {
    case class Done[A](a: A) extends IO[A]

    case class More[A](f: () => IO[A]) extends IO[A]

    case class FlatMap[A, B](ta: IO[A], f: A => IO[B]) extends IO[B]

    def suspend[A](a: => A): IO[A] =
      More(() => Done(a))
  }

  val xx = IO.suspend(println("hello"))
  xx.run
}
