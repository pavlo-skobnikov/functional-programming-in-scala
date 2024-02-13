import scala.annotation.tailrec

// Rational class
class Rational(x: Int, y: Int):
  def num = if y < 0 then -x else x
  def den = Math.abs(y)

  def add(r: Rational): Rational = Rational(num * r.den + r.num * den, den * r.den)
  def sub(r: Rational): Rational = add(r.neg)
  def mul(r: Rational): Rational = Rational(num * r.num, den * r.den)
  def div(r: Rational): Rational = mul(Rational(r.den, r.num))
  def neg: Rational = Rational(-num, den)

  def simplify(): Rational = Rational(num / gcd, den / gcd)

  lazy val gcd = num * den / lcm
  lazy val lcm =
    @tailrec
    def loop(multiple: Int): Int =
      if (multiple % num == 0 && multiple % den == 0) then multiple
      else loop(multiple + lowestAbs)

    loop(lowestAbs)

  lazy private val lowestAbs = Math.abs(if num < den then num else den)

  override def toString(): String = s"${num}/${den}"

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)

x.add(y).mul(z)
x.sub(y).div(z)
