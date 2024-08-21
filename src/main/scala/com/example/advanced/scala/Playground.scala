package com.example.advanced.scala


object Playground extends App {
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  val absO: Option[Double] => Option[Double] = lift(math.abs)
  val x = List(1, 2, 3, 4, 5) match {
    case x::_ => x

    case List(a, b) => 101
  }
  println(x)
}
