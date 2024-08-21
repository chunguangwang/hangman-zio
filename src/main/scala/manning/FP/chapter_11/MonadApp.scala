package manning.FP.chapter_11

import com.example.advanced.scala.PureState.State

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MonadApp extends App {
  // map(x)(a => a) == x
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def condistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }

  object Functor {
    val listFunctor = new Functor[List] {
      def map[A, B](as: List[A])(f: A => B): List[B] = as map f
    }
  }
  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    def map[A, B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))


    // For `List`, the `replicateM` function will generate a list of lists.
    // It will contain all the lists of length `n` with elements selected from the
    // input list.
    // For `Option`, it will generate either `Some` or `None` based on whether the
    // input is `Some` or `None`. The `Some` case will contain a list of length `n`
    // that repeats the element in the input `Option`.
    // The general meaning of `replicateM` is described very well by the
    // implementation `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value
    // `n` times and gathers the results in a single value, where the monad `M`
    // determines how values are actually combined.

    // Recursive version:
    def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)

    // Using `sequence` and the `List.fill` function of the standard library:
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms match {
        case Nil => unit(Nil)
        case h :: t => flatMap(f(h))(b =>
          if (!b) filterM(t)(f)
          else map(filterM(t)(f))(h :: _)
        )
      }

    // monad law
    // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
      a => flatMap(f(a))(g) // equivalent to (a: A) => flatMap(f(a))(fa => g(fa))
      // We can now state the associative law for monads in a much more symmetric way:
      //        compose(compose(f, g), h) == compose(f, compose(g, h))
    }

    def flatMapComposeVersion[A, B](ma: F[A])(f: A => F[B]): F[B] =
      compose((_:Unit) => ma, f)(())

    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
  }
  val set = HashSet(Array.fill(3)(2))
  set.toList

  object Monad {

    val optionMonad = new Monad[Option] {
      def unit[A](a: => A) = Some(a)

      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
    }

    val streamMonad = new Monad[Stream] {
      def unit[A](a: => A) = Stream(a)

      override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
    }

    val listMonad = new Monad[List] {
      def unit[A](a: => A) = List(a)

      override def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
    }

    // Since `State` is a binary type constructor, we need to partially apply it
    // with the `S` type argument. Thus, it is not just one monad, but an entire
    // family of monads, one for each type `S`. One solution is to create a class
    // `StateMonads` that accepts the `S` type argument and then has a _type member_
    // for the fully applied `State[S, A]` type inside:
    class StateMonads[S] {
      type StateS[A] = State[S, A]

      // We can then declare the monad for the `StateS` type constructor:
      val monad = new Monad[StateS] {
        def unit[A](a: => A): State[S, A] = State(s => (a, s))

        override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
          st flatMap f
      }
    }

    // But we don't have to create a full class like `StateMonads`. We can create
    // an anonymous class inline, inside parentheses, and project out its type member,
    // `lambda`:
    def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  case class State[S, A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        (f(a), s1)
      })
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })

    def getState[S]: State[S, S] = State(s => (s, s))

    def setState[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  type IntState[A] = State[Int, A]
  object IntStateMonad1 extends Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State(s => (a, s))
    def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
      st flatMap f
  }
  // equivalently
  object IntStateMonad2 extends Monad[({type IntState[A] = State[Int, A]}) # IntState] {
    override def unit[A](a: => A): State[Int, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]): State[Int, B] =
      ma flatMap f
  }

  def stateMonad[S]: Monad[({
    type f[x] = State[S, x]
  })#f] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  /*
  replicateM for State repeats the same state transition a number of times and returns a list of the results.
  It's not passing the same starting state many times,
  but chaining the calls together so that the output state of one is the input state of the next.

  map2 works similarly in that it takes two state transitions
  and feeds the output state of one to the input of the other.
   The outputs are not put in a list, but combined with a function f.

  sequence takes an entire list of state transitions and does the same kind of thing as replicateM:
  it feeds the output state of the first state transition to the input state of the next, and so on.
  The results are accumulated in a list.
   */

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] = as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
    xs <- acc
    n <- acc.getState
    _ <- acc.setState(n + 1)
  } yield (n, a) :: xs).run(0)._1.reverse
  println(zipWithIndex(List('b', 'c', 'c')))

  case class Reader[R, A](run: R => A)

  import cats.implicits._

  val v1 = "error".invalid[Option[String]]
  val v2 = Some("abc").valid[String]

  println(v1.fold(identity, _.getOrElse("")))
  println(v2.fold(identity, _.getOrElse("")))
}
