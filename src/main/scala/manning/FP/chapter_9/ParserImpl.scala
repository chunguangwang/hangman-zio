package manning.FP.chapter_9

import scala.util.matching.Regex

/*
The advantages of algebraic design
When you design the algebra of a library first,
representations for the data types of the algebra don’t matter as much.
As long as they support the required laws and functions,
you don’t even need to make your representations public.
There’s an idea here that a type is given meaning based on its relationship to other types
 (which are specified by the set of functions and their laws), rather than its inter- nal representation.
  This viewpoint is often associated with category theory,
  a branch of mathematics we’ve mentioned before.
  See the chapter notes for more on this con- nection if you’re interested.
 */
/*
 string(s)—Recognizes and returns a single String
 slice(p)—Returns the portion of input inspected by p if successful
 succeed(a)—Always succeeds with the value a
 map(p)(f)—Applies the function f to the result of p, if successful
 product(p1,p2)—Sequences two parsers, running p1 and then p2, and returns
the pair of their results if both succeed
 or(p1,p2)—Chooses between two parsers, first attempting p1, and then p2 if
p1 fails
 */
object ParserImpl extends App {
  trait Parser[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]
    def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
    def or[A](s1: => Parser[A], s2: Parser[A]): Parser[A]
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0) succeed(List())
      else map2(p, listOfN(n - 1, p))(_ :: _)
    /*
    examples:
    run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
     */

    /*
    This parser always succeeds with the value a,
    regardless of the input string (since string("") will always succeed, even if the input is empty).
     */
    def succeed[A](a: A): Parser[A] = string("").map(_ => a)

    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

    /*
     * In general, we expect map to be structure preserving much like we required for Par
    and Gen. Let’s formalize this by stipulating the now-familiar law:
            map(p)(a => a) == p
     * */
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(a => succeed(f(a)))

    /*
    we can now define our parser like this
    map(many(char('a')))(_.size)
    now we can write the same thing with nicer syntax
    val numA: Parser[Int] = char('a').many.map(_.size)

    We expect that, for instance, run(numA)("aaa") gives Right(3), and run(numA)("b") gives Right(0)
     */

    def slice[A](p: Parser[A]): Parser[String]
    /*
    We call this combinator slice since we intend for it
    to return the portion of the input string examined by the parser if successful.
    As an example, run(slice(('a'|'b') .many))("aaba") results in Right("aaba")
    — we ignore the list accumulated by many
    and simply return the portion of the input string matched by the parser.

    With slice, our parser that counts 'a' characters can now be written as char('a') .many.slice.map(_.size)
    (assuming we add an alias for slice to ParserOps)
    here is now referencing the size method on String, which takes constant time, rather than the size method on List,
    which takes time proportional to the length of the list (and requires us to actually construct the list).
     */

    /*
    Note that there’s no implementation here yet.
    We’re still just coming up with our desired interface.
    But slice does put a constraint on the implementation,
    namely, that even if the parser p.many.map(_.size) will generate an intermediate list when run,
    slice(p.many).map(_.size) will not. This is a strong hint that slice is primitive,
    since it will have to have access to the internal representation of the parser.
     */

    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p.flatMap(a => p2.map(b => (a, b)))

    def **[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = product(p, p2)

    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      // map(product(p, p2))(f.tupled)
      for {
        a <- p
        b <- p2
      } yield f(a, b)

    /*
    We could introduce a combinator, wrap:

    def wrap[A](p: => Parser[A]): Parser[A]
    Then define many as:

    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, wrap(many(p)))(_ :: _) or succeed(List())
    In the parallelism chapter, we were particularly interested in avoiding having Par objects
    that took as much time and space to build as the corresponding serial computation,
    and the delay combinator let us control this more carefully. Here, this isn't as much of a concern,
    and having to think carefully each time we map2 to decide
    whether we need to call wrap seems like unnecessary friction for users of the API.
     */
    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)

    /*
    With many1, we can now implement the parser for zero or more 'a' followed by one
    or more 'b' as follows:
    char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
     */

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
    /*
    Can you see how this signature implies an ability to sequence parsers where each
    parser in the chain depends on the output of the previous one?

    We now have an even smaller set of just six primitives: string, regex, slice, succeed, or, and flatMap.
    But we also have more power than before. With flatMap, instead of the less-general map and product,
    we can parse not just arbitrary context- free grammars like JSON,
     but context-sensitive grammars as well,
     including extremely complicated ones like C++ and PERL!
     */

    /*
    for {
      digit <- "[0-9]+".r
      val n = digit.toInt
      // we really should catch exceptions thrown by toInt
      // and convert to parse failure
      _ <- listOfN(n, char('a'))
    } yield n
     */
    implicit def regex(r: Regex): Parser[String]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
      ParserOps(f(a))
    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def many: Parser[List[A]] = self.many(p)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def slice: Parser[String] = self.slice(p)

      def many1: Parser[List[A]] = self.many1(p)
      def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    }
  }
}
