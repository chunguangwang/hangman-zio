package com.example


import zio.ZIOAppDefault

import java.io.FileInputStream

object ZIOScopedEx extends ZIOAppDefault {
//  val resource1: ZIO[Scope, Nothing, FileInputStream] = {
//    ZIO.acquireRelease(ZIO.debug("Acquiring the resource 1") file.open("dd.txt"))(_ =>
//      ZIO.debug("Releasing the resource one") *> ZIO.sleep(5.seconds) file.close()
//    )
//  }
//
//  val zioTest = for {
//    r1 <- resource1
//    _ <- ZIO.succeed(r1.readNBytes())
//  } yield()
//
//
//  val resource2: ZIO[Scope, Nothing, Unit] =
//    ZIO.acquireRelease(ZIO.debug("Acquiring the resource 2"))(_ =>
//      ZIO.debug("Releasing the resource two") *> ZIO.sleep(3.seconds)
//    )
//
//  def run =
//    ZIO.scoped(
//      for {
//        scope <- ZIO.scope
//        _     <- ZIO.debug("Entering the main scope!")
//        _     <- scope.addFinalizer(ZIO.debug("Releasing the main resource!") *> ZIO.sleep(2.seconds))
//        _     <- scope.extend(resource1)
//        _ <- ZIO.sleep(3.seconds)
//        _     <- scope.extend(resource2)
//        _     <- ZIO.debug("Leaving scope!")
//      } yield ()
//    )
//    zioTest
  import zio._

  import java.io.IOException
  import scala.io._

  def acquire(name: => String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(Source.fromFile(name))

  def release(source: => Source): ZIO[Any, Nothing, Unit] =
    ZIO.succeedBlocking(source.close())

  def source(name: => String): ZIO[Scope, IOException, Source] =
    ZIO.acquireRelease(acquire(name))(release(_))

//  source("cool.txt").flatMap { source =>
//    ZIO.attemptBlockingIO(source.getLines())
//  }

  def contents(name: => String): ZIO[Any, IOException, Chunk[String]] =
    ZIO.scoped {
      source(name).flatMap { source =>
        ZIO.attemptBlockingIO(Chunk.fromIterator(source.getLines()))
      }
    }

  def run = for {
    _ <- ZIO.succeed("success")
    inputStreamOuter <- ZIO.scoped {for {
    inputStream <- source(
      "/Users/chunguang.wang/IdeaProjects/Hangman/src/main/resources/cool.text"
    )
    dd <- ZIO
      .attemptBlockingIO(Chunk.fromIterator(inputStream.getLines()))
    _ <- ZIO.succeed(dd.foreach(println))
    aa = inputStream
    cc <- ZIO
      .attemptBlockingIO(Chunk.fromIterator(inputStream.getLines()))
    _ <- ZIO.succeed(cc.foreach(println))
//    _ <- contents(
//      "/Users/chunguang.wang/IdeaProjects/Hangman/src/main/resources/cool.text"
//    ).flatMap(println)
//    _ <-
  } yield (inputStream)}
    _ <- ZIO.succeed(inputStreamOuter)
  } yield ()
}
