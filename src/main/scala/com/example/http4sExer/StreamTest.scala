package com.example.http4sExer

import cats.data.Kleisli
import cats.effect.{ExitCode, IOApp, Sync}
import com.comcast.ip4s.IpLiteralSyntax
import org.http4s.{Request, Response}
import org.http4s.ember.server.EmberServerBuilder

object StreamTest extends IOApp {
  import cats.effect.IO
  import org.http4s.HttpRoutes
  import org.http4s.implicits._
  import fs2.Stream

  import org.http4s.dsl.io._
  import scala.concurrent.duration._

  //  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  def entryPoint[F[_]: Sync] = {
    import natchez.log.Log
    import org.typelevel.log4cats.Logger
    import org.typelevel.log4cats.slf4j.Slf4jLogger
    implicit val log: Logger[F] = Slf4jLogger.getLogger[F]
    Log.entryPoint[F]("foo")
  }

  // An infinite stream of the periodic elapsed time
  val seconds: Stream[IO, FiniteDuration] =
    Stream.awakeEvery[IO](1.second).take(40)

  val routes = HttpRoutes.of[IO] {
    case GET -> Root / "seconds" =>
      Ok(
        seconds.map(_.toString)
      ) // `map` `toString` because there's no `EntityEncoder` for `Duration`
    case GET -> Root / "hello" / name =>
      Ok(s"Hello $name")
  }

  val services: Kleisli[IO, Request[IO], Response[IO]] = routes.orNotFound
  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(services)
    .build
    .use(_ => IO.never)

  override def run(args: List[String]): IO[ExitCode] = server
}
