package lectures.cats.book.chapter10

import cats.implicits.catsSyntaxValidatedId
import lectures.cats.book.chapter10.ValidationRedesign1.Predicate.{
  And,
  Or,
  Pure
}

object ValidationRedesign1  extends App {

  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._ // for mapN
  import cats.data.Validated._ // for Valid and Invalid

  sealed trait Predicate[E, A] {
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

  import cats.Semigroup
  import cats.data.Validated

  sealed trait Check[E, A, B] {

    import Check._

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, f)

    def flatMap[C](f: B => Check[E, A, C]) =
      FlatMap[E, A, B, C](this, f)

    def andThen[C](next: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, next)
  }

  object Check {
    final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C)
        extends Check[E, A, C] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a) map func
    }

    final case class FlatMap[E, A, B, C](
        check: Check[E, A, B],
        func: B => Check[E, A, C]
    ) extends Check[E, A, C] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
    }

    final case class AndThen[E, A, B, C](
        check: Check[E, A, B],
        next: Check[E, B, C]
    ) extends Check[E, A, C] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => next(b).toEither))
    }

    final case class Pure[E, A, B](func: A => Validated[E, B])
        extends Check[E, A, B] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
        func(a)
    }

    final case class PurePredicate[E, A](pred: Predicate[E, A])
        extends Check[E, A, A] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        pred(a)
    }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      PurePredicate(pred)

    def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] =
      Pure(func)
  }

  import cats.data.{NonEmptyList, Validated}
  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)
  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.length > n
    )
  val alphanumeric: Predicate[Errors, String] = Predicate.lift(
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

  import cats.syntax.apply._ // for mapN
  import cats.syntax.validated._ // for valid and invalid

//  Here’s the implementation of checkUsername:

  // A username must contain at least four characters
  // and consist entirely of alphanumeric characters

  val checkUsername: Check[Errors, String, String] =
    Check(longerThan(3) and alphanumeric)

  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.

  val splitEmail: Check[Errors, String, (String, String)] =
    Check({
      val dd: Predicate[Errors, String] = Predicate.lift(
        error("Must contain a single @ character"),
        aa =>
          aa.split('@') match {
            case Array(name, domain) => true
            case _ => false
          }
      )
      dd
    }).map(s =>
      s.split('@') match {
        case Array(name, domain) =>
          (name, domain)
        case _ => ("", "")
      }
    )


  val splitAndJoinEmail: Check[Errors, String, String] =
    Check({
      val dd: Predicate[Errors, String] = Predicate.lift(
        error("Must contain a single @ character"),
        aa =>
          aa.split('@') match {
            case Array(name, domain) => true
            case _                   => false
          }
      )
      dd
    })

  val checkLeft: Check[Errors, String, String] =
    Check(longerThan(0))

  val checkRight: Check[Errors, String, String] =
    Check(longerThan(3) and contains('.'))

  val checkRightPredicate = longerThan(3) and contains('.')

  val joinEmail: Check[Errors, (String, String), String] =
    Check { case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

  val checkEmail: Check[Errors, String, String] =
    splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(
                  username: String,
                  email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)

  println(createUser("Noel", "noel@underscore.io"))
  // res5: Validated[Errors, User] = Valid(User("Noel", "noel@underscore.io"))
  println(createUser("", "dave@underscore.io@io"))
}
