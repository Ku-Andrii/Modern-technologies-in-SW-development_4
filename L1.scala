import scala.language.implicitConversions
import org.scalacheck.Prop.forAll

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
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

object L1 {
  def main(args: Array[String]): Unit = {
    implicit def mops[A](a: A)(implicit g: Monoid[A]): MonoidOps[A] = {
      new MonoidOps[A](a)
    }
    val empty = ""
    val assoc = forAll { (a: String, b: String, c: String) =>
      (a ** b) ** c == a ** (b ** c)
    }
    val emp = forAll { (a: String) =>
      a ** empty == a
    }
    assoc.check()
    emp.check()
  }
}