
trait TaglessBinary[T] {

  def zero(next: Option[T]): T
  def one(next: Option[T]): T

}

/*interpreter to convert a binary number to decimal*/
class toDecimal(in : Int) extends TaglessBinary[Int] {
    //not implemented yet

    def zero(next: Option[Int]): Int = 0

    def one(next: Option[Int]): Int = 1
}



/*an object to simplify calling TaglessBinary constructors*/
object BinSyntax {

    def zero[T](next: Option[T]) (implicit e: TaglessBinary[T]): T = e.zero(next)

    def one[T](next: Option[T]) (implicit e: TaglessBinary[T]): T = e.one(next)

}

import BinSyntax._ 

object test {
    def main(args: Array[String]) : Unit ={
        def bin1[T: TaglessBinary]: T = one(Some(zero(Some(one(None)))))

    }
}
