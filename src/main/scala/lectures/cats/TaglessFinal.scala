package lectures.cats

object TaglessFinal extends App {

  // expression problem
  object ExpressionProblem {

    object TaglessInitial {
      trait Expr[A]
      case class B(boolean: Boolean) extends Expr[Boolean]
      case class Or(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
      case class And(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
      case class Not(expr: Expr[Boolean]) extends Expr[Boolean]
      case class I(int: Int) extends Expr[Int]
      case class Sum(left: Expr[Int], right: Expr[Int]) extends Expr[Int]

      def eval[A](expr: Expr[A]): A = expr match {
        case B(b: A) => b
        case I(i: A) => i
        case Or(left, right) => eval(left) || eval(right)
        case Sum(left: A, right: A) => eval(left) + eval(right)
        // etc
      }
    }

  }

  def demoTagless(): Unit = {
    import lectures.cats.TaglessFinal.ExpressionProblem.TaglessInitial._
    println(eval(Or(B(true), And(B(true), B(false)))))
    println(eval(Sum(I(24), I(-3))))
  }

  demoTagless()

  object TaglessFinal {
    trait Expr[A] {
      val value: A // the final value we care about
    }

    def b(boolean: Boolean): Expr[Boolean] = new Expr[Boolean] {
      val value = boolean
    }
    def i(int: Int): Expr[Int] = new Expr[Int] {
      val value = int
    }
    def or(left: Expr[Boolean], right: Expr[Boolean]) = new Expr[Boolean] {
      val value = left.value || right.value
    }
    def and(left: Expr[Boolean], right: Expr[Boolean]) = new Expr[Boolean] {
      val value = left.value && right.value
    }
    def sum(left: Expr[Int], right: Expr[Int]) = new Expr[Int] {
      val value = left.value + right.value
    }
    def eval[A](expr: Expr[A]): A = expr.value
  }

  def demoTaglessFinal(): Unit = {
    import TaglessFinal._
    println(eval(or(b(true), and(b(true), b(false)))))
    println(eval(sum(i(24), i(-3))))
  }

  demoTaglessFinal()

  trait Algebra[E[_]] {
    def b(boolean: Boolean): E[Boolean]
    def i(int: Int): E[Int]
    def or(left: E[Boolean], right: E[Boolean]): E[Boolean]
    def and(left: E[Boolean], right: E[Boolean]): E[Boolean]
    def sum(left: E[Int], right: E[Int]): E[Int]
  }

  case class SimpleExpr[A](value: A)

  implicit val simpleExprAlg: Algebra[SimpleExpr] = new Algebra[SimpleExpr] {
    override def b(boolean: Boolean) = SimpleExpr (boolean)
    override def i(int: Int) = SimpleExpr (int)
    override def or(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]) = SimpleExpr (left.value || right.value)
    override def and(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]) = SimpleExpr (left.value && right.value)
    override def sum(left: SimpleExpr[Int], right: SimpleExpr[Int]) = SimpleExpr (left.value + right.value)
  }

  def program1: SimpleExpr[Boolean] = {
    import lectures.cats.TaglessFinal.simpleExprAlg.{and, b, i}
    simpleExprAlg.or(b(true), and(b(true), b(false)))
  }

  def program2: SimpleExpr[Int] = {
    import lectures.cats.TaglessFinal.simpleExprAlg.{and, b, i}

    simpleExprAlg.sum(i(24), i(-3))
  }

  println(program1.value)
  println(program2.value)
}
