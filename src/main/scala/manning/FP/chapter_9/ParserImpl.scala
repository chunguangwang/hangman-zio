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
 regex(s)—Recognizes a regular expression s
 slice(p)—Returns the portion of input inspected by p if successful
 label(e)(p)—In the event of failure, replaces the assigned message with e
 scope(e)(p)—In the event of failure, adds e to the error stack returned by p
 flatMap(p)(f)—Runs a parser, and then uses its result to select a second
parser to run in sequence
 attempt(p)—Delays committing to p until after it succeeds
 or(p1,p2)—Chooses between two parsers, first attempting p1, and then p2 if p1
fails in an uncommitted state on the input
 */
object ParserImpl extends App {
  trait Parsers[ParseError, Parser[+_]] { self =>
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
    // The intended meaning of label is that if p fails,
    // its ParseError will somehow incorpo- rate msg
    def label[A](msg: String)(p: Parser[A]): Parser[A]
    case class Location(input: String, offset: Int = 0) {
      lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
      lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
        case -1        => offset + 1
        case lineStart => offset - lineStart
      }
    }
    def errorLocation(e: ParseError): Location
    def errorMessage(e: ParseError): String

    def scope[A](msg: String)(p: Parser[A]): Parser[A]

    /*
    Is the label combinator sufficient for all our error-reporting needs? Not quite. Let’s look at an example:
    val p = label("first magic word")("abra") ** " ".many **
            label("second magic word")("cadabra")
    Skip whitespace
    What sort of ParseError would we like to get back from run(p)("abra cAdabra")?
    (Note the capital A in cAdabra.)
    The immediate cause is that capital 'A' instead of the expected lowercase 'a'.
     */
    trait Parser[+A] {
      def run(input: String): Either[ParseError, A]
    }
    implicit def regex(r: Regex): Parser[String] = new Parser[String] {
      def run(input: String): Either[ParseError, String] =
        r.findPrefixOf(input) match {
          case Some(matched) => Right(matched)
          case None          => Left(ParseError(List((Location(input), s"Expected pattern: ${r.regex}"))))
        }
    }
    case class ParseError(stack: List[(Location, String)])

    /*
    Let’s look at a more concrete motivating example:
    val spaces = " ".many
    val p1 = scope("magic spell") {
              "abra" ** spaces ** "cadabra"
            }
    val p2 = scope("gibberish") { "abba" ** spaces ** "babba"
    }
    val p = p1 or p2
    What ParseError would we like to get back from run(p)("abra cAdabra")?
    (Again, note the capital A in cAdabra.)
    Both branches of the or will produce errors on the input.

    One common solution to this problem is to have all parsers commit by default
    if they examine at least one character to produce a result.12
    We then introduce a combinator, attempt, which delays committing to a parse:
     */

    def attempt[A](p: Parser[A]): Parser[A]
    /*
    It should satisfy something like this:
      attempt(p flatMap (_ => fail)) or p2 == p2
    Here fail is a parser that always fails (we could introduce this as a primitive combinator if we like).

    The attempt combinator can be used whenever there’s ambiguity in the grammar
    and multiple tokens may have to be examined
    before the ambiguity can be resolved and parsing can commit to a single branch.
    As an example, we might write this:
    (attempt("abra" ** spaces ** "abra") ** "cadabra") or (
             "abra" ** spaces "cadabra!")
     */

    /**
     * In the event of an error, returns the error that occurred after consuming
     * the most number of characters.
     */
    def furthest[A](p: Parser[A]): Parser[A]

    /** In the event of an error, returns the error that occurred most recently. */
    def latest[A](p: Parser[A]): Parser[A]

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

  /*
  Our JSON parser doesn’t need to know the internal details of how parsers are represented.
  We can simply write a function that produces a JSON parser using only the set of primitives we’ve defined and any derived combinators.
  That is, for some JSON parse result type (we’ll explain the JSON format and the parse result type shortly),
  we’ll write a function like this:
  def jsonParser[ParseError, Parser[+_]](P: Parsers[ParseError, Parser]): Parser[JSON]
   */

}
