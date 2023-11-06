package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    @tailrec
    def findTargetVal(currRow: List[Int], prevRow: List[Int]): Int =
      val currRowIdx = prevRow.length
      val colToCalc = currRow.length
      def nextRowVal = if colToCalc == 0 || colToCalc == currRowIdx then 1 else
        prevRow(colToCalc - 1) + prevRow(colToCalc)

      if colToCalc == c && currRowIdx == r then nextRowVal
      else if currRow.length > prevRow.length then findTargetVal(List(), currRow)
      else findTargetVal(currRow :+ nextRowVal, prevRow)

    findTargetVal(List(), List())

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def balance(chars: List[Char], count: Int): Boolean = chars match
      case _ if count < 0 => false
      case Nil => count == 0
      case head :: tail if head == '(' => balance(tail, count + 1)
      case head :: tail if head == ')' => balance(tail, count - 1)
      case _ => balance(chars.tail, count)

    balance(chars, 0)


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def countChangeInner(money: Int, coins: List[Int]): Int =
      if (money < 0 || coins.isEmpty) then 0
      else if (money == 0) then 1
      else countChangeInner(money, coins.tail) + countChangeInner(money - coins.head, coins)

    countChangeInner(money, coins)
