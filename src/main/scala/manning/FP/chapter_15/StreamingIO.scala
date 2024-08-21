package manning.chap_15

import manning.chap_15.StreamingIO.Process.{Await, Emit, Halt, liftOne}

object StreamingIO extends App {
  trait Process[I, O] {
    def apply(s: LazyList[I]): LazyList[O] =
      this match {
        case Halt() => {
          println("00")
          LazyList()
        }
        case Await(recv) =>
          s match {
            case h #:: t => {
              recv(Some(h))(t)
            }
            case xs => recv(None)(xs) // Stream is empty
          }
        case Emit(h, t) =>
          h #:: t(s)
      }

    private def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] =
        p match {
          case Halt() => go(this)
          case Await(recv) =>
            Await {
              case None => recv(None)
              case i => {
                val dd = go(recv(i))
                dd
              }
            }
          case Emit(h, t) => Emit(h, go(t))
        }
      go(this)
    }
  }
  object Process {

    case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]())
        extends Process[I, O]

    case class Await[I, O](recv: Option[I] => Process[I, O])
        extends Process[I, O]

    case class Halt[I, O]() extends Process[I, O]

    def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
      Emit(head, tail)

    def liftOne[I, O](f: I => O): Process[I, O] =
      Await {
        case Some(i) => Emit(f(i))
        case None    => Halt()
      }

    def lift[I, O](f: I => O): Process[I, O] =
      liftOne(f).repeat

  }

  val p: Process[Int, Int] = liftOne((x: Int) => x * 2)
  val xs = p(LazyList(1, 2, 3)).toList
//  val xss = p(LazyList()).toList
  println(xs)
  /* detailed calculation steps
  p(Stream(1, 2, 3))
= Await {
  case Some(i) => Emit (i * 2)
  case None => Halt()
} Stream(1, 2, 3)
= case Some(1) => Emit(2, Halt())(Stream(2, 3))
= Emit(2, Halt())(Stream(2, 3))
= Stream(2) #:: Halt()(Stream(2, 3))
= Stream(2)Stream()
= Stream(2)
   */

//  val units = LazyList.continually(())
//  private val ones = Process.lift((x: Int) => x * 5)(LazyList(1, 2, 3))
//  println(ones.toList)
//  println(Halt()(LazyList(3)))
  /*
  lift((x: Int) => x * 2)(Stream(1, 2, 3))
= Await {
  case Some(i) => Emit(i * 2)
  case None => Halt()
}.repeat
(Stream(1, 2, 3))
= Await {
  case None => {
    Halt()
  }
  case i => {
    go(Emit(i * 2))
  }
} (Stream(1, 2, 3))
= go(Emit(1 * 2))(Stream(2, 3))
= Emit(2, go(Halt()))(Stream(2, 3))
= 2 #:: go(Await)(Stream(2, 3))
= 2 #:: Await {
    case None => {
      Halt()
    }
    case i => {
      go(Emit(i * 2))
    }(Stream(2, 3))
= 2 #:: go(Emit(2 * 2))(Stream(3)
= 2 #:: Emit(4, go(Halt())(Stream(3))
= 2 #:: 4 #:: go(Halt())(Stream(3))
= 2 :: 4 :: go(Await)(Stream(3))
= 2 :: 4 :: Await {
      case None => {
        Halt()
      }
      case i => {
        go(Emit(i * 2))
      }(Stream(3))
= 2 :: 4 :: go(Emit(3 * 2))(Stream())
= 2 :: 4 :: Emit(6, go(Halt())(Stream())
= 2 :: 4 :: 6 #:: go(Halt())(Stream())
= 2 :: 4 :: 6 #:: go(Await)(Stream())
= 2 :: 4 :: 6 #:: Await {
        case None => {
          Halt()
        }
        case i => {
          go(Emit(i * 2))
        }(Stream())
= 2 :: 4 :: 6 #:: Halt()
= 2 :: 4 :: 6 #:: Stream()
= 2 :: 4 :: 6


// recv = {
  case None => {
    Halt()
  }
  case i => {
    go(Emit(i * 2))
  }
}
   */
}
