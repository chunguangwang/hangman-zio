package manning.FP.chapter_7
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{atomic, Callable, ExecutorService, Executors, ThreadPoolExecutor}
import annotation.tailrec

/** Processes messages of type `A`, once at a time. Messages are submitted to the actor with the method `!`. Processing
  * is typically performed asynchronously, this is controlled by the provided `strategy`.
  *
  * Memory consistency guarantee: when each message is processed by the `handler`, any memory that it mutates is
  * guaranteed to be visible by the `handler` when it processes the next message, even if the `strategy` runs the
  * invocations of `handler` on separate threads. This is achieved because the `Actor` reads a volatile memory location
  * before entering its event loop, and writes to the same location before suspending
  *
  * @see
  *   scalaz.concurrent.Promise for a use case.
  *
  * @param handler
  *   The message handler
  * @param onError
  *   Exception handler, called if the message handler throws any `Throwable`.
  * @oaram
  *   strategy Execution strategy, for example, a strategy that is backed by an `ExecutorService`
  * @tparam A
  *   The type of message accepted by this actor
  */
final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw _) {
  self =>
  private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]
  private val tail = new AtomicReference({
    val cc = new Node[A]()
    val dd = cc.get()
    cc
  })
  private val suspended = new AtomicInteger(1)
  private val head = new AtomicReference({
    val dd: Node[A] = tail.get
    dd
  })

  /** Alias for `apply` */
  def !(a: A): Unit = {
    val n = new Node(a)
    val dd = head.getAndSet(n)
    dd.lazySet(n)
    trySchedule
  }

  private def trySchedule(): Unit =
    if (suspended.compareAndSet(1, 0)) schedule()

  def contramap[B](f: B => A): Actor[B] =
    new Actor[B](strategy)((b: B) => this ! f(b), onError)

  private def schedule(): () => Unit =
    strategy(act())

  private def act(): Unit = {
    val t = tail.get
    val n = batchHandle(t, 1024)
    if (n ne t) {
      n.a = null.asInstanceOf[A]
      tail.lazySet(n)
      schedule()
    } else {
      suspended.set(1)
      if (n.get ne null) trySchedule()
    }
  }

  @tailrec
  private def batchHandle(t: Actor.this.Node[A], i: Int): Node[A] = {
    val n: Node[A] = t.get // Matryoshka Doll
    if (n ne null) {
      try
        handler(n.a)
      catch {
        case ex: Throwable => onError(ex)
      }
      if (i > 0)
        batchHandle(n, i - 1)
      else
        n
    } else
      t
  }
}

object Actor {

  /** Create an `Actor` backed by the given `ExecutorService`. */
  def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw _): Actor[A] =
    new Actor(Strategy.fromExecutorService(es))(handler, onError)
}

/** Provides a function for evaluating expressions, possibly asynchronously. The `apply` function should typically begin
  * evaluating its argument immediately. The returned thunk can be used to block until the resulting `A` is available
  */
trait Strategy {
  def apply[A](a: => A): () => A
}

object Strategy {

  /** We can create a `Strategy` from any `ExecutorService`. It's a little more convenient thant submitting `Callable`
    * objects directly.
    */
  def fromExecutorService(es: ExecutorService): Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val f = es.submit(new Callable[A] { def call = a })
      () => f.get
    }
  }

  /** A `Strategy` which begins executing its argument immediately in the calling thread.
    */
  def sequential: Strategy = new Strategy {
    override def apply[A](a: => A): () => A = {
      val r = a
      () => r
    }
  }
}

object Ex extends App {
  val es = Executors.newFixedThreadPool(3)
  val ac = Actor(es)(
    (x: Int) => println("current thread: " + Thread.currentThread().getName + " " + x),
    (e: Throwable) => throw e
  )
  for (i <- 1 to 3_000)
    ac ! i
  println(5 - 4)

  import java.util.concurrent.TimeUnit

  es.shutdown
  try // Wait for existing tasks to complete
    if (!es.awaitTermination(60, TimeUnit.SECONDS)) { // Force shutdown if tasks are not completed within the timeout
      es.shutdownNow
      // Wait again for tasks to respond to being cancelled
      if (!es.awaitTermination(60, TimeUnit.SECONDS)) System.err.println("ExecutorService did not terminate")
    }
  catch {
    case e: InterruptedException =>
      // Re-cancel if current thread also interrupted
      es.shutdownNow
      // Preserve interrupt status
      Thread.currentThread.interrupt()
  }
}
