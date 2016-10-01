/**
 * Exercise 1
 * Purpose : Implementing the pascal function 
 *  which takes a column c and a row r, counting from 0 and returns the number at that spot in the triangle.
*/
def pascal (c: Int, r: Int ): Int = {

    def gen_list(pascal_row: List[Int], row: Int, prev: Int) : List[Int] = {

        if (pascal_row.isEmpty) return (1::Nil)

        else if (row == 0) return (pascal_row.head::pascal_row)

        else return ((pascal_row.head+prev)::gen_list(pascal_row.tail, row, pascal_row.head))

    }

    def rec_pascal(col: Int, row: Int, pascal_row: List[Int]) : Int = {

        if (row == r) {
            return pascal_row(c)
        }
        else 
            return rec_pascal(col, row + 1, gen_list(pascal_row, row, 0))
    }
    val pascal_row = List[Int](1)
    rec_pascal(0, 0, pascal_row)
}

