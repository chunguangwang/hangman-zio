//package lectures.advanced.scala.zio.streams
//
//import ZIOStreams.{
//  businessLogic,
//  defectStream,
//  failingStream,
//  nonFailingStream,
//  recoveryStream,
//  sink
//}
//import zio.Cause.{Die, Fail}
//import zio.{Chunk, ExitCode, Queue, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}
//import zio.stream.{ZPipeline, ZSink, ZStream}
//
//import java.io.{IOException, InputStream}
//
//object ZIOStreams {
//
//  val sum: ZSink[Any, Nothing, Int, Nothing, Int] =
//    ZSink.sum[Int]
//  val take5: ZSink[Any, Nothing, Int, Int, Chunk[Int]] =
//    ZSink.take[Int](5)
//  val take5Map: ZSink[Any, Nothing, Int, Int, Chunk[String]] =
//    take5.map(chunk => chunk.map(_.toString))
//
//  val stringStream: ZStream[Any, Nothing, String] =
//    ZStream("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
//
//  // We can use contramap to use our take5 logic, and operate on stringStream with it.
//  val take5Strings: ZSink[Any, Nothing, String, Int, Chunk[String]] =
//    take5Map.contramap[String](_.toInt)
//
//  val businessLogic: ZPipeline[Any, Nothing, String, Int] =
//    ZPipeline.map[String, Int](_.toInt)
//
//  val filterLogic: ZPipeline[Any, Nothing, Int, Int] =
//    ZPipeline.filter[Int](_ > 3)
//
//  val appLogic: ZPipeline[Any, Nothing, String, Int] =
//    businessLogic >>> filterLogic
//
//  val zio: ZIO[Any, Nothing, Int] =
//    stringStream.via(appLogic).run(sum)
//
//  val failStream: ZStream[Any, String, Int] =
//    ZStream(1, 2) ++ ZStream.fail("Abstract reason") ++ ZStream(4, 5)
//
//  class RealFakeInputStream[T <: Throwable](failAt: Int, failWith: => T)
//      extends InputStream {
//    val data: Array[Byte] = "0123456789".getBytes
//    var counter = 0
//
//    override def read(b: Array[Byte]): Int = {
//      if (counter == failAt) throw failWith
//      if (counter < data.length) {
//        b(0) = data(counter)
//        counter += 1
//        1
//      } else {
//        -1
//      }
//    }
//
//    // Not used, but needs to be implemented
//    override def read(): Int = ???
//  }
//
//  // 99 is higher than the length of our data, so we won't fail
//  val nonFailingStream: ZStream[Any, IOException, String] =
//    ZStream
//      .fromInputStream(
//        new RealFakeInputStream(99, new IOException("")),
//        chunkSize = 1
//      )
//      .map(b => new String(Array(b)))
//
//  // We will fail, and the error type matches ZStream error channel
//  val failingStream: ZStream[Any, IOException, String] =
//    ZStream
//      .fromInputStream(
//        new RealFakeInputStream(5, new IOException("")),
//        chunkSize = 1
//      )
//      .map(b => new String(Array(b)))
//
//  // We fail, but the error does not match the ZStream error channel
//  val defectStream: ZStream[Any, IOException, String] =
//    ZStream
//      .fromInputStream(
//        new RealFakeInputStream(5, new IndexOutOfBoundsException("")),
//        chunkSize = 1
//      )
//      .map(b => new String(Array(b)))
//
//  // When recovering, we will use this ZStream as the fall-back
//  val recoveryStream: ZStream[Any, Throwable, String] =
//    ZStream("a", "b", "c")
//
//  // We will pull values one at a time, and turn them into one string separated by "-"
//  val sink: ZSink[Any, Nothing, String, Nothing, String] =
//    ZSink.collectAll[String].map(_.mkString("-"))
//}
//
//object ZStreamExample extends ZIOAppDefault {
//
//  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
////    nonFailingStream
////      .run(sink)
////      .debug("sink")
////    failingStream
////      .orElse(recoveryStream)
////      .run(sink)
////      .debug("sink")
////    failingStream
////      .orElseEither(recoveryStream)
////      .run(ZSink.collectAll[Either[String, String]])
////      .debug("sink")
////    failingStream
////      .catchSome { case _: IOException =>
////        recoveryStream
////      }
////      .run(sink)
////      .debug("sink")
////    defectStream
////      .catchSomeCause {
////        case Fail(e: IOException, _)              => recoveryStream
////        case Die(e: IndexOutOfBoundsException, _) => recoveryStream
////      }
////      .run(sink)
////      .debug("sink")
////    failingStream
////      .catchAll {
////        case _: IOException => recoveryStream
////        case _              => ZStream("x", "y", "z")
////      }
////      .run(sink)
////      .debug("sink")
////    defectStream
////      .catchAllCause(_ => recoveryStream)
////      .run(sink)
////      .debug("sink")
////    failingStream.either
////      .run(ZSink.collectAll[Either[IOException, String]])
////      .debug("sink")
////    failingStream.either.collectRight.run(sink).debug
//    for {
//      queue <- Queue.unbounded[Int]
//      producer <- nonFailingStream
//        .via(businessLogic)
//        .run(ZSink.fromQueueWithShutdown(queue))
//        .debug
//        .fork
//      result <- ZStream
//        .fromQueue(queue)
//        .run(ZSink.sum[Int])
//        .debug("sum")
//        .fork
//      _ <- producer.join
//      _ <- result.join
//    } yield ExitCode.success
//}
