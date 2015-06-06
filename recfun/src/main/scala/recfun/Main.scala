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
  /*def pascal(c: Int, r: Int): Int = c match  {
    case 0 => 1
    case x=> r match{
      case 0 => 1
      case 1 => 1
      case y => pascal(c-1,r-1)+pascal(c,r-1)
    }
  }*/

  //Can use tuples....
  def pascal(c: Int, r: Int): Int = (c,r) match  {
    case (0,any) => 1
    case (any,1) => 1
    case (x,y) => pascal(c-1,r-1)+pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def iterate(chars:List[Char],count:Int):Boolean=chars match{
      case Nil => count==0
      case '('::tail => iterate(tail,count+1)
      case ')'::tail=> {
        if (count == 0) false
        else iterate(tail, count - 1)
      }
      case x::xs => iterate(xs,count)
    }

    iterate(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0,cl) => 1
    case (m, cl)  => {
      if (cl.isEmpty || m < 0) 0
      else countChange(m, cl.tail) + countChange(m - cl.head, cl)
    }
  }

}
