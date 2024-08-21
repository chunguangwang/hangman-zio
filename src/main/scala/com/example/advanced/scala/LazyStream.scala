package com.example.advanced.scala

import com.example.advanced.scala.LazyStream.Stream.{cons, empty}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object LazyStream extends App {
  sealed trait Stream[+A] {
    def toListRecursive: List[A] = this match {
      case Cons(h, t) => h()::t().toListRecursive
      case _ => List()
    }

    def toList: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List()).reverse
    }

    def toListFast: List[A] = {
      val buf = new ListBuffer[A]
      @tailrec
      def go(s: Stream[A]): List[A] = s match {
        case Cons(h, t) =>
          buf += h()
          go(t())
        case _ => buf.toList
      }
      go(this)
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), Stream.empty)
      case _ => Stream.empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](h: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty)
        empty
      else
        cons(as.head, apply(as.tail: _*))
  }

  val s = Stream.apply(3, 4, 5, 7)
  val p = s.drop(3).toListFast
  println(p)
}
