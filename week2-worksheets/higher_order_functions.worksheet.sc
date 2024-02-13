import scala.annotation.tailrec

// Implements (sigma) sum function using tail recursion
def sum(f: Int => Int, a: Int, b: Int): Int =
  @tailrec
  def loop(a: Int, acc: Int): Int =
    if a > b then acc
    else loop(a + 1, acc + f(a))

  loop(a, 0)

sum(x => x, 2, 4)
sum(x => x * x, 2, 4)
sum(x => x * x * x, 2, 4)


// Implements (pi) product function using tail recursion
def product(f: Int => Int, a: Int, b: Int): Int =
  @tailrec
  def loop(a: Int, acc: Int): Int =
    if a > b then acc
    else loop(a + 1, acc * f(a))

  loop(a, 1)

product(x => x, 2, 4)
product(x => x * x, 2, 4)
product(x => x * x * x, 2, 4)

// Defines factorial in terms of (pi) product
def factorial(x: Int): Int = product(x => x, 1, x)

factorial(4)
factorial(5)
factorial(6)

// Generalizes both mathematical sum and product functions
def accumulateApplyingToRange
    (accFunc: (Int, Int) => Int, identity: Int)
    (valFun: Int => Int)
    (rangeStart: Int, rangeEndInclusive: Int): Int =
  @tailrec
  def loop(iterationVal: Int, acc: Int): Int =
    if iterationVal > rangeEndInclusive then acc
    else loop(iterationVal + 1, accFunc(acc, valFun(iterationVal)))

  loop(rangeStart, identity)
end accumulateApplyingToRange

def factorial2(x: Int) = accumulateApplyingToRange((acc, value) => acc * value, 1)(x => x)(1, x)

factorial2(4)
factorial2(5)
factorial2(6)
