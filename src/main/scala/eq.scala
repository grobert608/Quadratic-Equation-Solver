import scala.io.StdIn._

object eq extends App{
  val s = new Solve
  s.solve
}

trait Coefficients{
  def coefficients:Array[Int] ={
    var coefficients = new Array[Int](0)
    try{
      coefficients = readLine.split(" ").map(_.toInt)
    } catch{
      case exception: NumberFormatException => {println("Введите коэффициенты через пробел!")
        coefficients = Array(0, 0, 1)}
    }
    coefficients
  }
}

class Solve extends Discriminant with Coefficients{
  val mas = coefficients
  val a:Double = mas(0)
  val b:Double = mas(1)
  val c:Double = mas(2)
  def solve:Unit = {
    if (a == 0) {
      if (b != 0) {
        def x:Double = -1 * (c / b)
        if (x > 0) print("x = " + x.toString.take(5))
        else print("x = " + x.toString.take(6))
      } else if (c == 0) print("x - любое число")
      else print("Нет решений")
    } else discriminant(a, b, c)
  }
}

trait Discriminant extends Output{
  def discriminant(a: Double, b: Double, c: Double):Unit = {
    val disc = math.pow(b, 2) - 4 * a * c
    if (disc > 0) {
      def x1:Double = (-b + math.sqrt(disc)) / (2 * a)
      if (x1 > 0) print("x1 = " + x1.toString.take(5))
      else print("x1 = " + x1.toString.take(6))
      def x2:Double = (-b - math.sqrt(disc)) / (2 * a)
      if (x2 > 0) print("; x2 = " + x2.toString.take(5))
      else print("; x2 = " + x2.toString.take(6))
    } else if (disc < 0) {
      val rational = (-b / (2 * a)).toString.take(5)
      val irrational = (math.sqrt(-disc) / (2 * math.abs(a))).toString.take(5)
      if ((-b / (2 * a)) == 0) print(s"x1 = i*$irrational; x2 = -i*$irrational")
      else print(s"x1 = $rational + i*$irrational; x2 = $rational - i*$irrational")
    } else if (disc == 0) {val x = -b/(2 * a); print(s"x1 = x2 = $x")}
  }
}

trait Output {
  def print(s: String):Unit = Console.print(s)
}