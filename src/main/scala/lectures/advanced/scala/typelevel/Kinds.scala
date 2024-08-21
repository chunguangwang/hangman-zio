package lectures.advanced.scala.typelevel

object Kinds extends App {

  // kind = type of types
  val aNumber: Int = 42 // level-0 types = kind
  case class Person(name: String, age: Int)
  val bob: Person = Person("Bob", 45)

  // "generic" = level-1 type
  class LinkedList[T] { // takes type arguments in the level-0 kind
    // code
  }

  val aList: LinkedList[Int] = ???
  //                    ^ level-0 type

  // level-2 types
  class Functor[F[_]]
  val functorList = new Functor[List]
  //                                  level-0 type

  // Functor = type constructor: [F[_]] => Functor[F]

  class Meta[F[_[_]]] // level-3 type
  val metaFunctor = new Meta[Functor]

  // examples
  class HashMap[K, V] // level-1
  val anAddressBook = new HashMap[String, String]

  class ComposedFunctor[F[_], G[_]] // level-2
  val aComposedFunctor = new ComposedFunctor[List, Option]

  class Formatter[F[_], T] // level-2...
  val aFormatter = new Formatter[Option, String]

}
