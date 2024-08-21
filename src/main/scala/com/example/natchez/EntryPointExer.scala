package com.example.natchez

import cats.data.Kleisli
import cats.effect.{ExitCode, IOApp, Sync}
import com.comcast.ip4s.IpLiteralSyntax
import org.http4s.{Request, Response}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT

object EntryPointExer extends IOApp {
  import cats.effect.IO
  import natchez.{EntryPoint, Kernel}
  import org.http4s.HttpRoutes
//  import org.http4s.implicits._

  import org.http4s.dsl.io._

//  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  def entryPoint[F[_]: Sync] = {
    import natchez.log.Log
    import org.typelevel.log4cats.Logger
    import org.typelevel.log4cats.slf4j.Slf4jLogger
    implicit val log: Logger[F] = Slf4jLogger.getLogger[F]
    Log.entryPoint[F]("foo")
  }
  def routes(ep: EntryPoint[IO]): HttpRoutes[IO] =
    HttpRoutes.of[IO] { case GET -> Root / "hello" / name =>
      ep.root("hello").use { span =>
        span.put("the-name" -> name) *> Ok(s"Hello, $name.")
      }
    }

  val services: Kleisli[IO, Request[IO], Response[IO]] = routes(entryPoint[IO]).orNotFound
  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(services)
    .build
    .use(_ => IO.never)

  override def run(args: List[String]): IO[ExitCode] = server
}
