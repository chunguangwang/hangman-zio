package lectures.cats.producerconsumer

import cats.effect.{Deferred, ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxParallelSequence1, showInterpolator}

import scala.concurrent.duration.FiniteDuration

object DeferredExample extends IOApp {
  def start(d: Deferred[IO, Int]): IO[Unit] = {
    val attemptCompletion: Int => IO[Unit] = n => IO.sleep(FiniteDuration(100, java.util.concurrent.TimeUnit.MILLISECONDS)) >> d.complete(n).void

    List(
      IO.race(attemptCompletion(1), attemptCompletion(2)),
      d.get.flatMap { n => IO(println(show"Result: $n")) }
    ).parSequence.void
  }

  val program: IO[Unit] =
    for {
      d <- Deferred[IO, Int]
      _ <- start(d)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = program.map(_ => ExitCode.Success)
}
