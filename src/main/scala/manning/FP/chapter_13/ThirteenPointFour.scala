package manning.FP.chapter_13

import fpinscala.iomonad.IO3.Console.{printLn, readLn}
import fpinscala.iomonad.IO3.{~>, consoleToReader, runConsole, runConsoleFunction0, runConsoleReader, runFree, Console, ConsoleReader, Free}
import manning.FP.chapter_7.Nonblocking.Par

object ThirteenPointFour extends App {
  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)
    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a)  => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) =>
      x match {
        case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
        case _          => sys.error("Impossible; `step` eliminates these cases")
      }
  }

//  val f1: Free[Console, Option[String]] = for {
//    _ <- printLn("I can only interact with the console.")
//    ln <- readLn
//  } yield ln
//  runConsole(f1)
//  runConsoleFunction0(f1)()
//  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
//  def forever[A, B, F[_]](a: F[A]): F[B] = {
//    lazy val t: F[B] = forever(a)
//    a flatMap (_ => t) = new IO[B] {def run = {a.run; forever(a)}.run}
//  }
  val consoleToReader =
    new (Console ~> ConsoleReader) { def apply[A](a: Console[A]) = a.toReader }

  val dd = runFree[Console, ConsoleReader, Option[String]](readLn)(consoleToReader)(ConsoleReader.monad)
  println(dd.run("ddddd"))
}
