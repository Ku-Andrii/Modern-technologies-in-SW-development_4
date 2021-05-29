import scala.language.implicitConversions

trait Monoid[A] {
  def combine(x: A, y: A): A
  def empty: A
}

object Monoid {
  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y: String): String = x ++ y
  }
}

class MonoidOps[A: Monoid](a: A)(implicit g: Monoid[A]){
  def **(b: A): A = g.combine(a, b)
}

object MonoidOps {
  implicit def mops[A](a: A)(implicit g: Monoid[A]): MonoidOps[A] = {
    new MonoidOps[A](a)
  }
}