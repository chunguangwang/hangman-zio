package com.example

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, ZoneId}
import java.util.Currency
import scala.math.BigDecimal.RoundingMode

object Test extends App {

  private def calculateProratedAmountExistingCycle(
      invoiceStartDate: Instant,
      invoiceEndDate: Instant
  ): BigDecimal = {
    val localInvoiceStartDate =
      LocalDate.ofInstant(invoiceStartDate, ZoneId.of("UTC"))
    val localInvoiceEndDate =
      LocalDate.ofInstant(invoiceEndDate, ZoneId.of("UTC"))
    val daysRemaining =
      ChronoUnit.DAYS.between(localInvoiceStartDate, localInvoiceEndDate)
    daysRemaining
  }

  val start = Instant.parse("2023-12-18T23:23:28.762367Z")
  val end = Instant.parse("2024-01-08T23:23:13.087Z")
  println(calculateProratedAmountExistingCycle(start, end))
  val str = "132272889"
  val intValueOption = str.toInt
  println(intValueOption)

//  sealed trait IO[A] {
//    def flatMap[B](f: A => IO[B]): IO[B] =
//      FlatMap(this, f)
//
//    def map[B](f: A => B): IO[B] =
//      flatMap(f andThen (Return(_)))
//  }
//
//  case class Return[A](a: A) extends IO[A]
//
//  case class Suspend[A](resume: () => A) extends IO[A]
//
//  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

//  @annotation.tailrec
//  def run[A](io: IO[A]): A = io match {
//    case Return(a)  => a
//    case Suspend(r) => r()
//    case FlatMap(x, f) =>
//      x match {
//        case Return(a)     => run(f(a))
//        case Suspend(r)    => run(f(r()))
//        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
//      }
//  }
//  val f: Int => IO[Int] = (x: Int) => Return(x)
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B])
      extends TailRec[B]

  @annotation.tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }
}
