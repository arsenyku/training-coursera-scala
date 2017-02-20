package recfun

import scala.collection.mutable.ListBuffer

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
  def pascal(c: Int, r: Int): Int =
  {
    val row = pascalIter(List(), r + 1)
    return row(c)
  }

  def pascalIter(previous: List[Int], maxLength: Int): List[Int] =
  {
    if (previous.size >= maxLength)
    {
      return previous
    }

    var row = ListBuffer[Int]()
    var left: Int = 0
    var right: Int = 0
    for (i <- 0 to previous.size - 1)
    {
      right = previous(i)

      row += left + right

      left = right
    }

    row += 1

    return pascalIter(row.toList, maxLength)

  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean =
  {
    return balanceIter(0, chars) == 0
  }

  def balanceIter(balanceValue: Int, chars: List[Char]): Int =
  {
    if (chars.size < 1 || balanceValue < 0)
    {
      return balanceValue
    }

    val first = chars(0)
    val rest = chars.drop(1)
    var newBalance = balanceValue

    if (first == '(')
    {
      newBalance = balanceValue + 1
    }
    else if (first == ')')
    {
      newBalance = balanceValue - 1
    }

    return balanceIter(newBalance, rest)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
  {
    val answer = countChangeIter(0, money, money, coins.sortWith((x, y) => x > y))
    return answer
  }

  def countChangeIter(waysToCount: Int, remainingChange: Int, originalMoney: Int, coins: List[Int]): Int =
  {
    if (coins.isEmpty)
    {
      return waysToCount
    }

    val head = coins.head
    val tail = coins.tail

    val newRemainingChange = remainingChange - head

    var result = waysToCount

    if (newRemainingChange < 0)
    {
      result += countChangeIter(waysToCount, remainingChange, originalMoney, tail)
    }
    else //if (newRemainingChange > 0)
    {
      if (newRemainingChange == 0) { result += 1 }

      result += countChangeIter(waysToCount, newRemainingChange, originalMoney, coins) +
        countChangeIter(waysToCount, remainingChange, originalMoney, tail)
    }

    return result

  }

}
