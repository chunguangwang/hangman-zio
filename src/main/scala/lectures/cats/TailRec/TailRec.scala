import cats._
import cats.implicits._

object TailRec extends App {
  val optionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](x: A): Option[A] = Some(x)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case Some(Right(b)) => Some(b)
      case Some(Left(a1)) => tailRecM(a1)(f)
      case None => None
    }
  }

  def iterateWhileM[A](initial: A)(f: A => Option[A])(p: A => Boolean): Option[A] =
    optionMonad.tailRecM(initial) { a =>
      if (p(a)) f(a).map(Left.apply)
      else None
    }

  iterateWhileM(1)(n => Some(n + 1))(_ < 1000) // Some(1000)
}
