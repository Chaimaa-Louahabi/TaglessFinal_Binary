/*binary = |One * Option(binary)
           |Zero * Option(binary)
*/

/*Scala Int : 32 bit signed value. Range -2147483648 to 2147483647
this ADT is only for positif integers
*/



trait Binary {

    /*Convert a scala integer to Binary */
    def toBinary(dec: Int) : Binary = {
        def toBinary(decimal: Int, bin: Binary) : Binary = {
            decimal match {
            case 0 => bin
            case _ => 
                if (decimal%2 == 0) {
                    toBinary(decimal/2, Zero(Some(bin)))
                } else {
                    toBinary(decimal/2, One(Some(bin)))
                }
            }
        }
        if (dec%2 == 0) {
            toBinary(dec, Zero(None))
        } else {
            toBinary(dec, One(None))
        }
    }

    /*Convert a binary to a scala integer*/
    def toDecimal(bin: Binary) : Int = {
        def toDecimal(bin: Binary, n: Int) : Int = bin match {
            case One(None) => 1
            case Zero(None) => 0
            case One(Some(next)) => (1 << (n-1)) + toDecimal(next, n-1)//scala.math.pow returns a double :(
            case Zero(Some(next)) => toDecimal(next, n-1)
        }
        toDecimal(bin, bin.size)
    }

     private def size: Int = this match {
        case Zero(Some(next)) => 1 + next.size
        case One(Some(next)) => 1 + next.size
        case Zero(None) => 1
        case One(None) => 1
    }

}
case class One(next: Option[Binary]) extends Binary
case class Zero(next: Option[Binary]) extends Binary

object test {

    def main(args: Array[String]) : Unit = {
        //test case 1
        val bin1 : Binary= One(Some(One(Some(Zero(None)))))
        val dec1 : Int = bin1.toDecimal(bin1)
        assert(dec1 == 6)
        //test case 2
        val bin2 : Binary= Zero(Some(Zero(None)))
        val dec2 : Int = bin2.toDecimal(bin2)
        assert(dec2 == 0)
        
        //val bin2bis :Binary= dec2.toBinary(dec2)
        //assert(bin2 == bin2bis)

    }
}

