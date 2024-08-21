package com.example.http4sExer

import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxParallelTraverse1
import org.http4s.{Request, Response}
import org.http4s.client.{Client, JavaNetClientBuilder}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits.http4sLiteralsSyntax

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object SimpleClient extends IOApp {
  def callEffect(client: Client[IO], str: String): IO[String] =
    client.expect[String](uri"http://localhost:8080" / str)

//  val blockingEC = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
  val httpClient: Client[IO] = JavaNetClientBuilder[IO].create

  def callStreamEffect(str: String): fs2.Stream[IO, Response[IO]] =
    httpClient.stream(req = Request(uri = uri"http://localhost:8080" / str))
  def hello(str: String): IO[String] =
    httpClient.expect[String](uri"http://localhost:8080/hello" / str)

  private val people = Vector("Michael", "Jessica", "Ashley", "Christopher")
  // people: scala.collection.immutable.Vector[String] = Vector(Michael, Jessica, Ashley, Christopher)

  private val greetingList = people.parTraverse(hello)
  val greetingsStringEffect = greetingList.map(_.mkString("\n")).map(println)
  override def run(args: List[String]): IO[ExitCode] = {
    val dd = callStreamEffect("seconds")
      .flatMap(_.body)
      .through(fs2.text.utf8Decode)
      .map(ss => {
        println(ss)
        ss
      })
      .compile
      .toList
      .map(_.mkString(("\n")))
    dd.map(ss => println("final string is: \n" + ss)).as(ExitCode.Success)
  }
//    BlazeClientBuilder[IO](scala.concurrent.ExecutionContext.Implicits.global).resource
//      .use { client =>
//        println(callEffect(client, "seconds").unsafeRunSync())
//        IO.unit
//      }
//      .as(ExitCode.Success)

//  greetingsStringEffect.as(ExitCode.Success)

}
