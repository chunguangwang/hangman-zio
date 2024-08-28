package com.example

import java.util.concurrent.{Callable, ExecutorService, Executors, Future}

object ExecutorExample {
  def main(args: Array[String]): Unit = {
    // Create two ExecutorServices
    val originalExecutor: ExecutorService = Executors.newFixedThreadPool(2)
    val secondaryExecutor: ExecutorService = Executors.newSingleThreadExecutor()

    try {
      // Submit a Callable to the secondary executor
      val futureResult: Future[Int] = secondaryExecutor.submit(new Callable[Int] {
        def call() = {
          println(s"Task is running on thread ${Thread.currentThread().getName}")
          val dd = 42 // Simulate some computation and return a result
          originalExecutor.submit(new Runnable {
            def run(): Unit = {
              val result = dd // Blocking call to get the result
              println(s"Result $result received on thread ${Thread.currentThread().getName}")
            }
          })
          42 * 2
        }
      })

      // When the result is ready, submit it back to the original executor
      println(futureResult.get())
    } finally {
      // Shutdown both executor services
      originalExecutor.shutdown()
      secondaryExecutor.shutdown()
    }
  }
}
