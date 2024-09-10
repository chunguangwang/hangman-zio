package manning.FP.chapter_14

object LocalEffect extends App {
  sealed trait ST[S, A] { self =>
    protected def run(s: S): (A, S)
    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }

    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S, A](a: => A) = {
      lazy val memo = a
      new ST[S, A] {
        def run(s: S) = (memo, s)
      }
    }
    def runST[A](st: RunnableST[A]): A =
      st.apply[Int].run(1)._1
    // st.apply[Unit].run(())._1
  }

  sealed trait STRef[S, A] {
    protected var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        cell = a
        ((), s)
      }
    }
  }

  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
      var cell = a
    })
  }

  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  val p = new RunnableST[(Int, Int)] {
    def apply[S]: ST[S, (Int, Int)] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      x1 <- r1.write(y + 1)
      y1 <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }
  val r = ST.runST(p)
  println(r)

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]
    def size: ST[S, Int] = ST(value.size)

    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }
    def read(i: Int): ST[S, A] = ST(value(i))
    def freeze: ST[S, List[A]] = ST(value.toList)
  }
  object STArray {
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST(
      new STArray[S, A] {
        lazy val value = Array.fill(sz)(v)
      }
    )
  }
}
