package lectures.cats

import cats.data.Kleisli

object KleisliEx extends App {
  // Bring in cats.FlatMap[Option] instance
  import cats.syntax.all._

  val parse: Kleisli[Option,String,Int] =
    Kleisli((s: String) => if (s.matches("-?[0-9]+")) Some(s.toInt) else Some(2))

  val reciprocal: Kleisli[Option,Int,Double] =
    Kleisli((i: Int) => if (i != 0) Some(1.0 / i) else Some(-1.0))

  val parseAndReciprocal: Kleisli[Option,String,Double] =
    parse.andThen(reciprocal)

  println(parseAndReciprocal("dd").get)
}
