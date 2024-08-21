package lectures.advanced.scala.exercises

import lectures.advanced.scala.exercises.FutureFP.Par.{fork, lazyUnit, run}

import java.util.concurrent.{Callable, ExecutorService, Executors, Future}
import scala.Console.println
import scala.concurrent.duration.TimeUnit

object FutureFP extends App {
  type Par[A] = ExecutorService => Future[A]
  trait ParT[A] {
    def unit[A](a: A): Par[A]

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

    def fork[A](a: => Par[A]): Par[A]

  }

  object Par {
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a,_) => f(a))
    def fork[A](a: => Par[A]): Par[A] = es => es.submit(
      new Callable[A] {
        def call = a(es).get
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

  val a = lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(4)
  println (Par.equal(S)(a, fork(a)))

  def sum(ints: IndexedSeq[Int]): Par[Int] = if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0) else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }

  val sumTest = sum(IndexedSeq(1, 2, 3, 4))
  println(run(S)(sumTest).get())
  S.shutdown()

}
