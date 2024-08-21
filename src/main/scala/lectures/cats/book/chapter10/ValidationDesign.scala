package lectures.cats.book.chapter10

object ValidationDesign extends App {
  import cats.Semigroup
  import cats.syntax.either._    // for asLeft and asRight
  import cats.syntax.semigroup._ // for |+|

  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] =
      func(a)

    def and(that: CheckF[E, A])
           (implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(e1),  Left(e2))  => (e1 |+| e2).asLeft
          case (Left(e),   Right(_))  => e.asLeft
          case (Right(_),  Left(e))   => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      }
  }

  // Let’s test the behaviour we get. First we’ll setup some checks:
  import cats.instances.list._ // for Semigroup
  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  private val check: CheckF[List[String], Int] =
    a and b
  println(check(5))
  println(check(0))

  val a1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)

  val b1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)

//  val check1 = a1 and b1
}
