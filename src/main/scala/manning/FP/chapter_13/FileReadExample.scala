package manning.FP.chapter_13

import java.nio.channels.AsynchronousFileChannel
import java.nio.file.{Paths, StandardOpenOption}
import manning.FP.chapter_7.Nonblocking.Par



object FileReadExample extends App {

  def read(file: AsynchronousFileChannel, fromPosition: Long, numBytes: Int): Par[Either[Throwable, Array[Byte]]] =
    Par.async { cb =>
      val buffer = java.nio.ByteBuffer.allocate(numBytes)
      file.read(
        buffer,
        fromPosition,
        null,
        new java.nio.channels.CompletionHandler[Integer, Null] {
          def completed(result: Integer, attachment: Null): Unit = {
            buffer.flip()
            val bytes = new Array[Byte](result)
            buffer.get(bytes)
            cb(Right(bytes))
          }

          def failed(exc: Throwable, attachment: Null): Unit =
            cb(Left(exc))
        }
      )
    }

  // Usage example

  val channel = AsynchronousFileChannel.open(Paths.get("example.txt"), StandardOpenOption.READ)
  val result = Par.run(java.util.concurrent.Executors.newSingleThreadExecutor())(read(channel, 0, 100))
  result match {
    case Right(bytes) => println(s"Read ${bytes.length} bytes: ${new String(bytes)}")
    case Left(error) => println(s"Error: $error")
  }

}
