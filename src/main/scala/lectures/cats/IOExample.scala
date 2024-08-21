package lectures.cats

import cats.effect.{ExitCode, IO, IOApp}

object IOExample extends IOApp {

//  def io(i: Int): IO[Unit] =
//    IO(println(s"Hi from $i!")) // short for IO.delay or IO.apply

  val program1 = for {
    _ <- io(1)
    _ <- io(2)
  } yield ExitCode.Success

  val program2 = for {
    fiber <- io(1).start
    _ <- io(2)
    _ <- fiber.join
  } yield ExitCode.Success

  def io(i: Int): IO[Unit] = IO({
    Thread.sleep(3000)
    println(s"Hi from $i!")
  })

  override def run(args: List[String]): IO[ExitCode] = program1
}
