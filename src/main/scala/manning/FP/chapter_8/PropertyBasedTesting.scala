package manning.FP.chapter_8

import com.example.advanced.scala.PureState.{RNG, SimpleRNG, State}
import manning.FP.chapter_8.PropertyBasedTesting.Prop.{FailedCase, SuccessCount}

object PropertyBasedTesting extends App {
  trait Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
    /* We can refer to the enclosing `Prop` instance with `Prop.this` */
    def &&(p: Prop): Prop = ???

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
  }

  case class Gen[A](sample: State[RNG,A]) {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
    }
    /* We could write this as an explicit state action, but this is far less
       convenient, since it requires us to manually thread the `RNG` through the
       computation. */
    def choose2(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(rng => RNG.nonNegativeInt(rng) match {
        case (n, rng2) => (start + n % (stopExclusive - start), rng2)
      }))

    def unit[A](a: => A): Gen[A] =
      Gen(State.unit(a))

    def boolean: Gen[Boolean] = ???
      //Gen(State(RNG.boolean))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))
  }
}
