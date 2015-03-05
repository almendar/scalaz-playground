package tk.playground.files

/**
 * Created by tomaszk on 2/16/15.
 */
trait Monoid[A] {
  def op(a1:A,a2:A) : A
  def zero : A
}


object Monoid {
  implicit val intMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1+a2
    override def zero: Int = 0
  }

  implicit def optionMonoid[A : Monoid] = new Monoid[Option[A]] {

    override def op(a1: Option[A], a2: Option[A]): Option[A] = (a1,a2) match {
      case (None, None) => None
      case (Some(x),Some(y)) => Some(implicitly[Monoid[A]].op(x,y))
      case (Some(x),None) => Some(x)
      case (None,Some(y)) => Some(y)
    }

    override def zero: Option[A] = None
  }

  implicit def endoMonoid[A] = new Monoid[A=>A] {

    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2

    override def zero: (A) => A = (x:A) => x
  }
}



object MonoidTest extends App {

  def shorten[A : Monoid](lst : List[A]) : A = lst.foldLeft(implicitly[Monoid[A]].zero)((arg1,arg2) => implicitly[Monoid[A]].op(arg1,arg2))

  println(shorten(List[Option[Int]](Some(1),Some(2),Some(3),Some(4),Some(5))))

  println(implicitly[Monoid[Option[Int]]].op(Some(1),(Some(2))))


}