package tk.playground.scalaz

import scalaz.{Free, Functor}

/**
 * Created by tomaszk on 2/15/15.
 */
sealed trait LogF[+A]


object Logging {
  case class Debug[A](msg: String, o: A) extends LogF[A]
  case class Info[A](msg: String, o: A) extends LogF[A]
  case class Warn[A](msg: String, o: A) extends LogF[A]
  case class Error[A](msg: String, o: A) extends LogF[A]



  implicit def logFFunctor[B]: Functor[LogF] = new Functor[LogF]{
    def map[A,B](fa: LogF[A])(f: A => B): LogF[B] =
      fa match {
        case Debug(msg,a) => Debug(msg,f(a))
        case Info(msg,a)  => Info(msg,f(a))
        case Warn(msg,a)  => Warn(msg,f(a))
        case Error(msg,a) => Error(msg,f(a))
      }
  }

}

object Println {
  import Logging._
  import scalaz.{~>,Id}, Id.Id

  type Log[A] = Free[LogF, A]

  private def write(prefix: String, msg: String): Unit =
    println(s"[$prefix] $msg")

  private def debug(msg: String): Unit = write("DEBUG", msg)
  private def info(msg: String): Unit  = write("INFO", msg)
  private def warn(msg: String): Unit  = write("WARN", msg)
  private def error(msg: String): Unit = write("ERROR", msg)

  private val exe: LogF ~> Id = new (LogF ~> Id) {
    def apply[B](l: LogF[B]): B = l match {
      case Debug(msg,a) => { debug(msg); a }
      case Info(msg,a) => { info(msg); a }
      case Warn(msg,a) => { warn(msg); a }
      case Error(msg,a) => { error(msg); a }
    }
  }

  def apply[A](log: Log[A]): A =
    log.runM(exe.apply[Log[A]])
}

