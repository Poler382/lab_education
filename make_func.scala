object  make_func {
    val rand = new scala.util.Random(0)
    def main(args: Array[String]): Unit = {
    }

    def ax_b(qnum:Int = 1,ran:Int)={
      var a = rand.nextInt(10)
      var b = rand.nextInt(10)
      var c = rand.nextInt(10)
      var x = 0f
      var xnum=100
      var n = true
      do{
        a = rand.nextInt(ran)+1
        b = rand.nextInt(ran)+1
        c = rand.nextInt(ran)+1


        x = (b-c)/a.toFloat
        var xn = x.toString.split('.').toArray
        if(xn.size == 2){
         xnum = xn(1).size
        }
        print(xnum)
        println(xnum > 3)
        println(a == b)
        println(a == c)
        println(b == c)
        println(a == 0)
        val n = (xnum >= 3 || a == b || a == c || b == c || a == 0 )
        println(n,a,b,c,x,xnum)
        //

      }while((xnum >= 3 || a == b || a == c || b == c || a == 0 ))

      println(a,b,c,x)
      var q = ""
      if(a == 1){
        q = "x+"+b.toString+" = "+c.toString
      }else{
        q = a.toString + "x+"+b.toString+" = "+c.toString
      }

      q
    }


}
