package recfun

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
  /* We know that pascal(c,r) = rCc = r! / (r-c)! / c!
     Also, pascal(c,r) = pascal(c-1,r) + pascal(c-1,r-1)
                           c-1 c
                            1
                           1 1
                    r-1 → 1 2 1
                     r → 1 3 3 1
                        1 4 6 4 1
                       1 5 0 0 5 1
    Using the recursive definition, we don't have the 
    inconvenience of dealing with huge numbers.
    The base cases are the extreme columns, which are
    all filled with 0s
  */
  def pascal(c: Int, r: Int): Int =
    if c <= 0 || c >= r then
      1
    else
      pascal(c,r-1) + pascal(c-1,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    def balanceInter(chars: List[Char], count: Int): Boolean = 
      if count < 0 then 
        false 
      else if chars.isEmpty then
        count == 0
      else
        var head = chars.head
        var tail = chars.tail
        if head == '(' then
          balanceInter(tail, count+1)
        else if head == ')' then 
          balanceInter(tail, count-1)
        else
          balanceInter(tail, count)
    balanceInter(chars,0)
      

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    /*Base cases*/
    if  money == 0 then
      1
    else if money < 0 || money > 0 && coins.isEmpty then
      0
    /*Recursive cases*/
    else 
      val firstCoin = coins.head
      val restCoins = coins.tail
      countChange(money-firstCoin, coins) +  // Use first coin
      countChange(money, restCoins)          // Don't use first coin