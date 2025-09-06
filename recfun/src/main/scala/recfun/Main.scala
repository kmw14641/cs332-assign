package recfun
import common._

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def processHead(chars: List[Char], openingNum: Int): Boolean = {
      if (openingNum < 0) false
      else if (chars.isEmpty) openingNum == 0
      else {
        val newOpeningNum = {
          if (chars.head == '(') openingNum + 1
          else if (chars.head == ')') openingNum - 1
          else openingNum
        }
        processHead(chars.tail, newOpeningNum)
      }
    }

    processHead(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeRecursive(money: Int, remainingCoins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (remainingCoins.isEmpty) 0
      else countChangeRecursive(money - remainingCoins.head, coins) + countChangeRecursive(money, remainingCoins.tail)
    }

    countChangeRecursive(money, coins)
  }
}
