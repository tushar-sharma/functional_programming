/**
   * Exercise 2
   * Write a recursive function which verifies the balancing of parentheses in a string, which we represent as a List[Char] not a String. For example, the function should return true for the following strings:

     ** (if (zero? x) max (/ 1 x))
     ** I told him (that it’s not (yet) done). (But he wasn’t listening)
   * The function should return false for the following strings:
     ** :-)
     ** ())(

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
}
