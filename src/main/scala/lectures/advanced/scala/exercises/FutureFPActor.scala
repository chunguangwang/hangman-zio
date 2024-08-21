package lectures.advanced.scala.exercises

import lectures.advanced.scala.exercises.FutureFPActor.Par.{fork, run}

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import scala.Console.println
import scala.concurrent.duration.TimeUnit
object FutureFPActor  extends  App {

    sealed trait Future[A] {
      private[FutureFPActor] def apply(k: A => Unit): Unit
    }

    type Par[A] = ExecutorService => Future[A]

    object Par {
      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

      def run[A](es: ExecutorService)(p: Par[A]): A = {
        val ref = new AtomicReference[A]
        val latch = new CountDownLatch(1)
        p(es) {
          a => ref.set(a); latch.countDown
        }
        latch.await
        ref.get
      }
      def unit[A](a: A): Par[A] = es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

      def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e) == p2(e)

      def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
        es => new Future[C] {
          def apply(cb: C => Unit): Unit = {
            var ar: Option[A] = None
            var br: Option[B] = None
            // this implementation is a little too liberal in forking of threads -
            // it forks a new logical thread for the actor and for stack-safety,
            // forks evaluation of the callback `cb`
            val combiner = Actor[Either[A, B]](es) {
              case Left(a) =>
                if (br.isDefined) eval(es)(cb(f(a, br.get)))
                else ar = Some(a)
              case Right(b) =>
                if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
                else br = Some(b)
            }
            p(es)(a => combiner ! Left(a))
            p2(es)(b => combiner ! Right(b))
          }
        }
      def map[A,B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a,_) => f(a))
      def fork[A](a: => Par[A]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            eval(es)(a(es)(cb))
        }
      def eval(es: ExecutorService)(r: => Unit): Unit =
        es.submit(new Callable[Unit] {
          def call = r
        })
      def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

      def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
      }

      // We define `sequenceBalanced` using `IndexedSeq`, which provides an
      // efficient function for splitting the sequence in half.
      def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
        if (as.isEmpty) unit(Vector())
        else if (as.length == 1) map(as.head)(a => Vector(a))
        else {
          val (l, r) = as.splitAt(as.length / 2)
          map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
        }
      }

      def sequence[A](as: List[Par[A]]): Par[List[A]] =
        map(sequenceBalanced(as.toIndexedSeq))(_.toList)

      def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
        val pars: List[Par[List[A]]] =
          l map (asyncF((a: A) => if (f(a)) List(a) else List()))
        // flatten is a convenience method on `List` for concatenating a list of lists
        map(sequence(pars))(_.flatten)
      }
      def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))
    }

    val a = FutureFPActor.Par.lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(4)
    println (Par.equal(S)(a, fork(a)))

    def sum(ints: IndexedSeq[Int]): Par[Int] = if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0) else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

    val sumTest = sum(IndexedSeq(1, 2, 3, 4))
    println(run(S)(sumTest))
    S.shutdown()

}
