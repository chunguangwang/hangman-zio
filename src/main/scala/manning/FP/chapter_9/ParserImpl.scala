package manning.FP.chapter_9

/*
The advantages of algebraic design
When you design the algebra of a library first,
representations for the data types of the algebra don’t matter as much.
As long as they support the required laws and func- tions,
you don’t even need to make your representations public.
There’s an idea here that a type is given meaning based on its relationship to other types
 (which are specified by the set of functions and their laws), rather than its inter- nal representation.
  This viewpoint is often associated with category theory,
  a branch of mathematics we’ve mentioned before.
  See the chapter notes for more on this con- nection if you’re interested.
 */
object ParserImpl extends App {
  trait Parser[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]
    def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
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

    def many[A](p: Parser[A]): Parser[List[A]]

    /*
     * In general, we expect map to be structure preserving much like we required for Par
    and Gen. Let’s formalize this by stipulating the now-familiar law:
            map(p)(a => a) == p
     * */
    def map[A, B](a: Parser[A])(f: A => B): Parser[B]

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

    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

    def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
      map(product(p, p2))(f.tupled)

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
      ParserOps(f(a))
    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def many: Parser[List[A]] = self.many(p)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def slice: Parser[String] = self.slice(p)
    }
  }
}
