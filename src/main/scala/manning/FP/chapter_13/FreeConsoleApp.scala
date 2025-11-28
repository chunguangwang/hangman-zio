package manning.FP.chapter_13

import fpinscala.iomonad.IO3._
import fpinscala.iomonad.IO3.Console._

object FreeConsoleApp extends App {
  val readHello = Console.readLn
  val readHello2 = Console.readLn.flatMap { s =>
    Console.printLn("Hello " + s)
  }
  val printRead: String => ConsoleIO[Unit] = (s: String) => Console.printLn(s)

  val freeConsole = for {
    s1 <- readHello
    _ <- readHello2
    _ <- printRead("s1 is: ++++  " + s1)
  } yield ()

  runConsole(freeConsole)
}