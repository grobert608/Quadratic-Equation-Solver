import org.scalatest.Matchers._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EqTest extends FlatSpec {
  "Linear equation" should "be correct" in {
    val sq = new Solve with MockOutput with FakeCoeffLin
    sq.solve
    sq.messages should contain ("x = -3.0")
  }
  "Any number" should "be correct result" in {
    val sq = new Solve with MockOutput with FakeCoeffAny
    sq.solve
    sq.messages should contain ("x - любое число")
  }
  "No solutions" should "be correct result" in {
    val sq = new Solve with MockOutput with FakeCoeffNot
    sq.solve
    sq.messages should contain ("Нет решений")
  }
  "Some number" should "be correct result" in {
    val sq = new Solve with MockOutput with FakeCoeffSome
    sq.solve
    sq.messages should contain ("x1 = -1.0 + i*1.414; x2 = -1.0 - i*1.414")
  }
}

trait FakeCoeffLin extends Coefficients {override def coefficients = Array(0, 1, 3)}

trait FakeCoeffAny extends Coefficients {override def coefficients = Array(0, 0, 0)}

trait FakeCoeffNot extends Coefficients {override def coefficients = Array(0, 0, 1)}

trait FakeCoeffSome extends Coefficients {override def coefficients = Array(1, 2, 3)}

trait MockOutput extends Output  {
  var messages: Seq[String] = Seq()
  override def print(s: String):Unit =messages = messages :+ s
}