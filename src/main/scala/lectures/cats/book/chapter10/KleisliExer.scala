package lectures.cats.book.chapter10

import cats.Semigroup
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.{
  catsSyntaxSemigroup,
  catsSyntaxTuple2Semigroupal,
  catsSyntaxValidatedId
}
import lectures.cats.book.chapter10.ValidationRedesign1.error

object KleisliExer extends App {
  import cats.data.Kleisli
  import cats.instances.list._ // for Monad

  val step1: Kleisli[List, Int, Int] =
    Kleisli(x => List(x + 1, x - 1))

  val step2: Kleisli[List, Int, Int] =
    Kleisli(x => List(x, -x))

  val step3: Kleisli[List, Int, Int] =
    Kleisli(x => List(x * 2, x / 2))

  val pipeline = step1 andThen step2 andThen step3

  println(pipeline.run(20))

  sealed trait Predicate[E, A] {
    import Predicate._
    def run(implicit s: Semigroup[E]): A => Either[E, A] =
      (a: A) => this(a).toEither
    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(_) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(_)    => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  object Predicate {
    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
        extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
        extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A])
        extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
      Pure(f)

    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
      Pure(a => if (fn(a)) a.valid else err.invalid)
  }

  import cats.data.{Kleisli, NonEmptyList}
  import cats.instances.either._ // for Semigroupal
  type Errors = NonEmptyList[String]

  type Result[A] = Either[Errors, A]

  type Check[A, B] = Kleisli[Result, A, B]

  // Create a check from a function:
  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  // Create a check from a Predicate:
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n
    )

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char)
    )

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1
    )

//  Our username and email examples are slightly different in that
  //  we make use of check() and checkPred() in different situations:

  val checkUsername: Check[String, String] =
    checkPred(longerThan(3) and alphanumeric)

  val splitEmail: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(name, domain) =>
        Right((name, domain))

      case _ =>
        Left(error("Must contain a single @ character"))
    })

  val checkLeft: Check[String, String] =
    checkPred(longerThan(0))

  val checkRight: Check[String, String] =
    checkPred(longerThan(3) and contains('.'))

  val joinEmail: Check[(String, String), String] =
    check { case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

  val checkEmail: Check[String, String] =
    splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(
                  username: String,
                  email: String): Either[Errors, User] = (
    checkUsername.run(username),
    checkEmail.run(email)
  ).mapN(User)

  println(createUser("Noel", "noel@underscore.io"))
  // res2: Either[Errors, User] = Right(User("Noel", "noel@underscore.io"))
  println(createUser("", "dave@underscore.io@io"))
  // res3: Either[Errors, User] = Left(
  //   NonEmptyList("Must be longer than 3 characters", List())
  // )
}
