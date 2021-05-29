import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import MonoidOps._

object MonoidSpecification extends Properties("Monoid") {
  val empty = ""
  property("assoc") = forAll { (a: String, b: String, c: String) =>
    (a ** b) ** c == a ** (b ** c)
  }
  property("emp") = forAll { (a: String) =>
    a ** empty == a && empty ** a == a
  }
}