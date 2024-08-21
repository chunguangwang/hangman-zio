package lectures.cats

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import lectures.cats.TrampolineExer.Trampoline.run

import scala.annotation.tailrec

object TrampolineExer extends App {
  trait Trampoline[+A]
  object Trampoline {
    case class Done[A](a: A) extends Trampoline[A]

    case class More[A](f: () => Trampoline[A]) extends Trampoline[A]

    case class FlatMap[A, B](ta: Trampoline[A], f: A => Trampoline[B])
        extends Trampoline[B]

    def resume[A](ta: Trampoline[A]): Either[() => Trampoline[A], A] =
      ta match {
        case Done(a)     => Right(a)
        case More(thunk) => resume(thunk())
        case FlatMap(t, f) =>
          t match {
            case Done(a2)     => resume(f(a2))
            case More(thunk2) => Left(() => FlatMap(thunk2(), f))
            case FlatMap(t2, f2) =>
              resume(FlatMap(t2, (x: Any) => FlatMap(f2(x), f)))
          }
      }

    @tailrec
    def run[A](ta: Trampoline[A]): A = resume(ta) match {
      case Right(a)    => a
      case Left(thunk) => run(thunk())
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): Trampoline[List[B]] =
      as match {
        case Nil => Done(Nil)
        case (h :: t) =>
          More { () =>
            FlatMap(flatMap(t)(f), (lb: List[B]) => Done(f(h) ::: lb))
          }
      }
  }

  import lectures.cats.TrampolineExer.Trampoline._

  @tailrec
  def fact(n: Int, acc: Int = 1): Int =
    if (n == 0) acc
    else fact(n - 1, n * acc)

  def isEven(n: Int): Trampoline[Boolean] =
    if (n == 0) Done(true)
    else More(() => isOdd(n - 1))

  def isOdd(n: Int): Trampoline[Boolean] =
    if (n == 0) Done(false)
    else More(() => isEven(n - 1))

  println(run(isEven(100_000)))
  val dd = flatMap[Int, Int]((1 to 1000_000).toList)(i => List(i, i + 1))
  println(run(dd))


  val greet: String => IO[Unit] = name => IO(println(s"Hello, $name!"))
  val helloResource: Resource[IO, String] = Resource.eval(IO.pure("World"))

  helloResource.use(greet).unsafeRunSync() // Prints: "Hello, World!"
}
