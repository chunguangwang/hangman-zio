package lectures.advanced.scala.exercises

import cats.Monoid

import java.util.concurrent.{Future, TimeUnit}


object ScalaFPChap1 extends App {
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1) {
      ints.headOption getOrElse 0
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

 case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                 f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.MILLISECONDS.convert(timeout, units))

    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis
        val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis;
        val at = stop - start
        val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }

    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      def op(f: A => A, g: A => A) = f compose g

      val zero = (a: A) => a

      override def empty: A => A = ???

      override def combine(x: A => A, y: A => A): A => A = ???
    }

    }

  val donuts: Seq[String] = Seq("Plain", "Strawberry", "Glazed")
  println(s"All donuts = ${donuts.foldRight("+++")((a, b) => a + "  aa  "+ b)}")
  val cc = Map[Int, Int]()
}
