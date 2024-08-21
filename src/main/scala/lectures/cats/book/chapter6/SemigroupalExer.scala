package lectures.cats.book.chapter6

object SemigroupalExer extends App {
  import cats.Semigroupal
  import cats.instances.option._ // for semigroupal

  println(Semigroupal[Option].product(Some(123), Some("abc")))
  // res1: Option[(Int, String)] = Some((123, "abc"))

  println(Semigroupal[Option].product(None, Some("abc")))
  // res2: Option[Tuple2[Nothing, String]] = None
  println(Semigroupal[Option].product(Some(123), None))
  // res3: Option[Tuple2[Int, Nothing]] = None
  println(Semigroupal.tuple3(Option(1), Option(2), Option(3)))
  // res4: Option[(Int, Int, Int)] = Some((1, 2, 3))
  println(Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]))
  // res5: Option[(Int, Int, Int)] = None
  println(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))
  println(Semigroupal.map2(Option(1), Option.empty[Int])(_ + _))

  // 6.2 Apply Syntax
  import cats.instances.option._ // for Semigroupal
  import cats.syntax.apply._ // for tupled and mapN
  (Option(123), Option("abc")).tupled
  // res8: Option[(Int, String)] = Some((123, "abc"))
  (Option(123), Option("abc"), Option(true)).tupled
  // res9: Option[(Int, String, Boolean)] = Some((123, "abc", true))

  final case class Cat(name: String, born: Int, color: String)

  (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat.apply)
  // res10: Option[Cat] = Some(Cat("Garfield", 1978, "Orange & black"))
  // Of all the methods mentioned here, it is most common to use mapN

  import cats.Monoid
  import cats.instances.int._ // for Monoid
  import cats.instances.invariant._ // for Semigroupal
  import cats.instances.list._ // for Monoid
  import cats.instances.string._ // for Monoid
  import cats.syntax.apply._ // for imapN

  final case class Cat1(
                        name: String,
                        yearOfBirth: Int,
                        favoriteFoods: List[String]
                      )

  val tupleToCat: (String, Int, List[String]) => Cat1 = (s, num, list) => Cat1(s, num, list)

  val catToTuple: Cat1 => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat1] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  import cats.syntax.semigroup._ // for |+|

  val garfield   = Cat1("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat1("Heathcliff", 1988, List("Junk Food"))

  println(garfield |+| heathcliff)
}
