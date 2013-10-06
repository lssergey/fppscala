package recfun
import common._

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
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], parens: List[Char]): Boolean = {
      if (chars.isEmpty) parens.isEmpty
      else {
        if (chars.head == '(') balanceIter(chars.tail, parens.::(')'))
        else if (chars.head == ')') {
          if (parens.isEmpty || ')' != parens.head) false
          else balanceIter(chars.tail, parens.tail)
        } else balanceIter(chars.tail, parens)
      }
    }

    balanceIter(chars, List())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.length == 0) 0
    else {
      def countIter(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0 || coins.length == 0) 0
        else countIter(money, coins.tail) + countIter(money - coins.head, coins)
      }

      countIter(money, coins.tail) + countIter(money - coins.head, coins)
    }
  }
}
