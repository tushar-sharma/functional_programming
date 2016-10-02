/**
  * Exercise 3
  * A recursive function that counts how many different ways you can make change for an amount, given a list of coin denominations. 
  * For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
  */
def countChange(money: Int, coins: List[Int]): BigInt = {
        //create a table to store previous values
        val table = Array.ofDim[BigInt](coins.length, money + 1)
        //initialize table's 1 column as 1
        for (row <- 0 to (coins.length - 1)) {
          for (col <- 0 to (money)) {
            if (col == 0) {
              table(row)(col) = 1
            }
            else table(row)(col) = 0
          }
        }

        var row = 0
         coins.foreach { e =>

          for (col <- 0 to money) {
            //special   /**
            * Exercise 2
            * (if (zero? x) max (/ 1 x))
            */
            def balance(chars: List[Char]): Boolean =  {
              val braces_stack = Stack[Char]()
              def isMatchingPair(arg1: Char, arg2: Char): Boolean = {
                if (arg1 == '(' && arg2 == ')')
                  return true
                else if (arg1 == '{' && arg2 == '}')
                  return true
                else if (arg1 == '[' && arg2 == ']')
                  return true
                else false
              }
              def recBalance(ls: List[Char]): Boolean = {
                if (ls.isEmpty) {
                  //check if stack is empty then braces are balanced
                  if (braces_stack.isEmpty) return true
                  else return false
                }
                //if opening braces, then push it into stack
                else if (ls.head == '(' || ls.head == '{' || ls.head == '[') {
                  braces_stack.push(ls.head)
                }
                else if (ls.head == ')' || ls.head == '}' || ls.head == ']') {
                  //return negative if empty
                  if (braces_stack.isEmpty) return false

                  val pop_value = braces_stack.pop

                  if (isMatchingPair(pop_value, ls.head) == false) return false
                }
                return recBalance(ls.tail)
              }
              recBalance(chars)
            }case for row 0
            if (row == 0) {
              if (col == 0) table(row)(col) = 1
              else if (col % e == 0) table(row)(col) = 1
              else  table(row)(col) = 0
            }
            else if (col >= e) {
              table(row)(col) = table(row - 1)(col) + table(row)(col - e)
            }
            else {
              table(row)(col) = table(row - 1)(col)
            }
          }
          row = row + 1
        }

         /* for (row <- 0 to (coins.length - 1)) {
          for (col <- 0 to (money)) {
            print(table(row)(col))
            print(" ")
          }
          println("")
        }*/
        return table(row -1)(money)
}
