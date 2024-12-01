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
    def char(c: Char): Parser[Char]
    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
    /*
    examples:
    run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
     */
    implicit def string(s: String): Parser[String]

//    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
      ParserOps(f(a))
    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]) = self.or(p, p2)
      def or[B >: A](p2: Parser[B]) = self.or(p, p2)
    }
  }
}
