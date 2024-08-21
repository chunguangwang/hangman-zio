package lectures.cats.book.chapter7

object FoldableAndTraverse extends App {
  // 7.1.4 Foldable in Cats
  import cats.Foldable
  import cats.instances.list._ // for Foldable

  val ints = List(1, 2, 3)
  println(Foldable[List].foldLeft(ints, 0)(_ + _))
  // res0: Int = 6

  // Other sequences like Vector and LazyList work in the same way.
  import cats.instances.option._ // for Foldable
  private val maybeInt = Option(123)
  println(Foldable[Option].foldLeft(maybeInt, 10)(_ * _))
  // res1: Int = 1230

  // 7.1.4.1 Folding Right
  // Foldable defines foldRight differently to foldLeft,
  // in terms of the Eval monad:
  //  def foldRight[A, B, F[_]](fa: F[A], lb: Eval[B])
  //  (f: (A, Eval[B]) => Eval[B]): Eval[B]

  // Using Eval means folding is always stack safe,
  // even when the collection’s default definition of foldRight is not.
  // For example, the default implementation of foldRight for LazyList is not stack safe.
  // The longer the lazy list, the larger the stack requirements for the fold.
  // A sufficiently large lazy list will trigger a StackOverflowError:
  import cats.Eval
  import cats.Foldable
  def bigData = (1 to 10000).to(LazyList)
  println(bigData.foldRight(0L)(_ + _))
  // could reproduce on Mac M1 with java.lang.StackOverflowError ...

  // Using Foldable forces us to use stack safe operations,
  // which fixes the overflow exception:
  import cats.instances.lazyList._ // for Foldable

  val eval: Eval[Long] = Foldable[LazyList]
    .foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }
  println(eval.value)

  // 7.1.4.2 Folding with Monoids
  // Foldable provides us with a host of useful methods defined
  // on top of foldLeft.
  // Many of these are facsimiles of familiar methods from the standard library:
  // find, exists, forall, toList, isEmpty, nonEmpty, and so on:
  println(Foldable[Option].nonEmpty(Option(42)))
  // res6: Boolean = true
  println(Foldable[List].find(List(4, 2, 3))(_ % 2 == 0))
  // res7: Option[Int] = Some(4)

  // In addition to these familiar methods, Cats provides two methods that make use of Monoids:
  //
  //combineAll (and its alias fold) combines all elements in the sequence using their Monoid;
  //
  //foldMap maps a user-supplied function over the sequence and combines the results using a Monoid.
  import cats.instances.int._ // for Monoid
  println(Foldable[List].combineAll(List(1, 2, 3)))
  println(
    Foldable[List]
      .foldRight(List(1, 2, 3), Eval.now(0L))((num, eval) => eval.map(num + _))
      .value
  )
  // res8: Int = 6

  // Alternatively, we can use foldMap to convert each Int to a String
  // and concatenate them:
  import cats.instances.string._ // for Monoid
  println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))
  // res9: String = "123"

  // Finally, we can compose Foldables
  // to support deep traversal of nested sequences:
  import cats.instances.vector._ // for Monoid
  val ints1 = List(Vector(1, 2, 3), Vector(4, 5, 6))
  println((Foldable[List] compose Foldable[Vector]).combineAll(ints1))
  // res11: Int = 21

  //  7.1.4.3 Syntax for Foldable

  import cats.syntax.foldable._ // for combineAll and foldMap

  List(1, 2, 3).combineAll
  // res12: Int = 6

  List(1, 2, 3).foldMap(_.toString)
  // res13: String = "123"

  // 7.2.1 Traversing with Futures
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )
  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  val allUptimes: Future[List[Int]] = {
    hostnames.foldLeft(Future(List.empty[Int])) { (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum <- accum
        uptime <- uptime
      } yield accum :+ uptime
    }
  }
  Await.result(allUptimes, 1.second)

  val allUptimes1: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)

  Await.result(allUptimes1, 1.second)
  // res2: List[Int] = List(1020, 960, 840)
  // This is essentially the same as our example code above. Future.traverse is abstracting away the pain of folding and defining accumulators and combination functions. It gives us a clean high-level interface to do what we want:
  //
  //start with a List[A];
  //provide a function A => Future[B];
  //end up with a Future[List[B]].

  // The standard library also provides another method, Future.sequence, that assumes we're starting
  // with a List[Future[B]] and don't need to provide an identity function:
  //  object Future {
  //    def sequence[B](futures: List[Future[B]]): Future[List[B]] =
  //      traverse(futures)(identity)
  //  }
  // In this case the intuitive understanding is even simpler:
  // • start with a List[Future[A]];
  // • end up with a Future[List[A]].

  // 7.2.2 Traversing with Applicatives
  // Future(List.empty[List])
  // is equivalent Applicative.pure:
  import cats.Applicative
  import cats.instances.future._ // for Applicative
  import cats.syntax.applicative._ // for pure

  List.empty[Int].pure[Future]

  def oldCombine(
      accum: Future[List[Int]],
      host: String
  ): Future[List[Int]] = {
    val uptime = getUptime(host)
    for {
      accum <- accum
      uptime <- uptime
    } yield accum :+ uptime
  }
  // is now equivalent to Semigroupal.combine:
  import cats.syntax.apply._ // for mapN

  // Combining accumulator and hostname using an applicative:
  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  def listTraverse[F[_]: Applicative, A, B](
      list: List[A]
  )(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  // We can use listTraverse to re-implement our uptime example:
  val totalUptime = listTraverse(hostnames)(getUptime)
  println(Await.result(totalUptime, 1.second))
  // res5: List[Int] = List(1020, 960, 840)

  // 7.2.2.1 Exercise: Traversing with Vectors
  import cats.instances.vector._ // for Applicative

  listSequence(List(Vector(1, 2), Vector(3, 4)))
  // res7: Vector[List[Int]] = Vector(
  //   List(1, 3),
  //   List(1, 4),
  //   List(2, 3),
  //   List(2, 4)
  // )
  listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  // res9: Vector[List[Int]] = Vector(
  //   List(1, 3, 5),
  //   List(1, 3, 6),
  //   List(1, 4, 5),
  //   List(1, 4, 6),
  //   List(2, 3, 5),
  //   List(2, 3, 6),
  //   List(2, 4, 5),
  //   List(2, 4, 6)
  // )

  // 7.2.2.2 Exercise: Traversing with Options
  import cats.instances.option._ // for Applicative
  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  // The arguments to listTraverse are of types List[Int] and Int => Option[Int],
  // so the return type is Option[List[Int]].
  // Again, Option is a monad, so the semigroupal combine function follows from flatMap.
  // The semantics are therefore fail-fast error handling: if all inputs are even,
  // we get a list of outputs. Otherwise we get None:
  process(List(2, 4, 6))
  // res12: Option[List[Int]] = Some(List(2, 4, 6))
  process(List(1, 2, 3))
  // res13: Option[List[Int]] = None

  // 7.2.2.3 Exercise: Traversing with Validated
  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  // The return type here is ErrorsOr[List[Int]],
  // which expands to Validated[List[String], List[Int]].
  // The semantics for semigroupal combine on validated are accumulating error handling,
  // so the result is either a list of even Ints,
  // or a list of errors detailing which Ints failed the test:
  type ErrorsOr[A] = Validated[List[String], A]
  def process1(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 2) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  process(List(2, 4, 6))
  // res17: ErrorsOr[List[Int]] = Valid(List(2, 4, 6))
  process(List(1, 2, 3))
  // res18: ErrorsOr[List[Int]] = Invalid(List("1 is not even", "3 is not even"))

  // 7.2.3 Traverse in Cats
  /*
  package cats
  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative, A, B](inputs: F[A])(func: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative, B](inputs: F[A]) =
      traverse(inputs)(identity)
  }
   */
}
