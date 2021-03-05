/*binary = |One * binary
           |Zero * binary
           |Nil
            
*/


trait Binary {

}
case class One(next: Binary) extends Binary
case class Zero(next: Binary) extends Binary
case class Nil() extends Binary

object test {

    /*Convert a scala integer to Binary */
    def int2bin(dec: Int) : Binary = {
        def toBinary(decimal: Int, rightBin: Binary) : Binary = {
            decimal match {
            case 0 => rightBin
            case _ => 
                if (decimal%2 == 0) {
                    toBinary(decimal/2, Zero(rightBin))
                } else {
                    toBinary(decimal/2, One(rightBin))
                }
            }
        }
            if (dec == 0 ) 
                Zero(Nil())
             else
                toBinary(dec,Nil())

        
    }

    /*Convert a binary to a scala integer*/
    def bin2int(bin: Binary) : Int = {
        def toDecimal(bin: Binary, len: Int) : Int = bin match {
            case One(Nil()) => 1
            case Zero(Nil()) => 0
            case One(next) => (1 << (len-1)) + toDecimal(next, len-1)//scala.math.pow returns a double :(
            case Zero(next) => toDecimal(next, len-1)
        }
        toDecimal(bin, size(bin))
    }

     private def size(bin : Binary): Int = bin match {
        case Zero(next) => 1 + size(next)
        case One(next) => 1 + size(next)
        case Nil() => 0
    }

    def main(args: Array[String]) : Unit = {
        //test case 1
        val bin110 : Binary= One(One(Zero(Nil())))

        assert(bin2int(bin110) == 6 )
        //test case 2
        val bin00 : Binary = Zero(Zero(Nil()))
        assert(bin2int(bin00) == 0)
        //test case 3
        val _bin00 :Binary= int2bin(0)
        assert(_bin00== Zero(Nil()))

        //make sure that forall x : bin2int(int2bin(x)) = x
        assert(bin2int(int2bin(310)) == 310)
        //make sure that forall x : int2bin(bin2int(x)) = x
        assert( int2bin(bin2int( bin110 )) == bin110 )

    }
}

