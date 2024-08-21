package lectures.advanced.scala.zio

object MyZio extends App {
  sealed trait Console[+A]
  final case class Return[A](value: () => A) extends Console[A]
  final case class PrintLine[A](line: String, rest: Console[A])
      extends Console[A]
  final case class ReadLine[A](rest: String => Console[A]) extends Console[A]

  def succeed[A](a: => A): Console[A] = Return(() => a)
  def printLine(line: String): Console[Unit] =
    PrintLine(line, succeed(()))
  val readLine: Console[String] =
    ReadLine(line => succeed(line))

  implicit class ConsoleSyntax[+A](self: Console[A]) {
    def map[B](f: A => B): Console[B] =
      flatMap(a => succeed(f(a)))

    def flatMap[B](f: A => Console[B]): Console[B] =
      self match {
        case Return(value) => f(value())
        case PrintLine(line, next) =>
          PrintLine(line, next.flatMap(f))
        case ReadLine(next) =>
          ReadLine(line => next(line).flatMap(f))
      }
  }

  val example1: Console[Unit] =
    PrintLine(
      "Hello, what is your name?",
      ReadLine(name =>
        PrintLine(s"Good to meet you, ${name}", Return(() => ()))
      )
    )

  val example2: Console[String] =
    for {
      _ <- printLine("What's your name?")
      name <- readLine
      _ <- printLine(s"Hello, ${name}, good to meet you!")
    } yield name

  val example3: Console[Unit] =
    for {
      name <- printLine("What's your name?")
    } yield name

  def interpret[A](program: Console[A]): A = program match {
    case Return(value) =>
      value()
    case PrintLine(line, next) =>
      println(line)
      interpret(next)
    case ReadLine(next) =>
      interpret(next(scala.io.StdIn.readLine()))
  }
  interpret(example1)
}
