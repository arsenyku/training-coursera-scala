package recfun

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
    row(c)
  }

  def pascalIter(previous: List[Int], maxLength: Int): List[Int] =
  {
    if (previous.size >= maxLength) previous
    else
    {
      val row = for (i <- 0 to previous.size - 1) yield
      {
        val left = if (i == 0) 0 else previous(i-1)
        val right = previous(i)
        left + right
      }
      pascalIter(row.toList ++ List(1), maxLength)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean =
  {
    balanceIter(0, chars) == 0
  }

  def balanceIter(balanceValue: Int, chars: List[Char]): Int =
  {
    if (chars.size < 1 || balanceValue < 0)
    {
      balanceValue
    }
    else
    {
      val first = chars(0)
      val rest = chars.drop(1)

      val newBalance =
        if (first == '(') balanceValue + 1
        else if (first == ')') balanceValue - 1
        else balanceValue

      balanceIter(newBalance, rest)

    }
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
  {
    countChangeIter(0, money, money, coins.sortWith((x, y) => x > y))
  }

  def countChangeIter(waysToCount: Int, remainingChange: Int, originalMoney: Int, coins: List[Int]): Int =
  {
    if (coins.isEmpty)
    {
      waysToCount
    }
    else
    {
      val head = coins.head
      val tail = coins.tail

      val newRemainingChange = remainingChange - head

      if (newRemainingChange < 0)
      {
        waysToCount + countChangeIter(waysToCount, remainingChange, originalMoney, tail)
      }
      else //if (newRemainingChange > 0)
      {
        val foundMatch = if (newRemainingChange == 0) 1 else 0

        foundMatch + countChangeIter(waysToCount, newRemainingChange, originalMoney, coins) +
          countChangeIter(waysToCount, remainingChange, originalMoney, tail)
      }
    }

  }

}
