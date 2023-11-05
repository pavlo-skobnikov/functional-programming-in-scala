import scala.annotation.tailrec
def and(x: Boolean, y: => Boolean) = if x then y else x

// Implements &&
and(true, true)
and(true, false)
and(false, true)
and(false, false)

def or(x: Boolean, y: => Boolean) = if x then x else y

// Implements ||
or(true, true)
or(true, false)
or(false, true)
or(false, false)

// Square Root
private def isGoodEnough(guess: Double, x: Double): Boolean =
  Math.abs(guess * guess - x) < 0.001
private def improveGuess(guess: Double, x: Double): Double =
  (guess + (x / guess)) / 2

def sqrt(x: Double): Double =
  @tailrec
  def sqrtIter(x: Double, guess: Double = 1.0): Double =
    if isGoodEnough(guess, x) then guess else sqrtIter(improveGuess(guess, x), x)

  sqrtIter(x)
end sqrt
