import math._
object  make_func {
  val rand = new scala.util.Random(0)
  def frac (a:Int,b:Int):String={
    var q = ""
    if(a > 0 && b > 0){
      q = """\frac{"""+str(a)+"}{"+str(b)+"}"
    }else if(a < 0 || b < 0){
      q = """- \frac{"""+str(abs(a))+"}{"+str(abs(b))+"}"
    }else{
      q = "miss"+str(a)+" "+str(b)
    }
    q
  }
  def Sfrac(a:String,b:String,A:Boolean):String={
    var q = ""
    if(A){
      q = """\frac{"""+a+"}{"+b+"}"
    }else{
      q = """- \frac{"""+a+"}{"+b+"}"
    }
    q
  }
  def num(range:Int)={
    if(rand.nextInt(10) % 3 == 0 ){
      rand.nextInt(range) / rand.nextInt(range).toFloat
    }else{
      rand.nextInt(range)
    }
  }

  def ans_change(q:String,x:Double,range:Int)={
    var xx = ""
    if(x % 1.0 == 0){
      xx = str(x.toInt)
    }else{
       xx = frac_change2(x,range*2)
    }  
    (q,xx)
  }

  def tester(){
    var ys = List[(String,String)]()

    ys ::= ones(1,10)
    ys ::= two(1,10)
    ys ::= three(1,10)
    ys ::= three_dash(1,10)
    ys ::= fore(1,10)
    ys ::= fore_dash(1,10)
    ys ::= five(1,10)

    ys.reverse.toArray.foreach{println(_)}

  }
  def main(args: Array[String]): Unit = {
    for(one <- 0 until 1){
      println("make "+one)

      var ys = List[(String,String)]()

      ys ::= ones(1,10)
      ys ::= two(1,10)
      ys ::= three(1,10)
      ys ::= fore(1,10)
      ys ::= five(1,10)
      ys ::= six(1,10)
      ys ::= six_dash(1,10)

      val ys1 =  ys.map(_._1).reverse.mkString("\n")+"\n"
      val as =  ys.map(_._2).reverse.mkString("\n")+"\n"

      val pathName = "test/question"+str(one)+".txt"
      val writer = new java.io.PrintWriter(pathName)

      val pathName2 = "test/ans"+str(one)+".txt"
      val writer2 = new java.io.PrintWriter(pathName2)

      writer.write(ys1)
      writer.close()

      writer2.write(as)
      writer2.close()

    }
  }
  def str (a:Int):String = a.toString
  def str (a:Double) :String= a.toString
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def random(a:Int) = if(rand.nextInt(2)==0) -1 * a else a
  //ax+b = c
  def ones(qnum:Int = 1,range:Int):(String,String) = {
    var a,b,c = 0
    var x = 0f
    var xnum=100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)

      x = (c-b)/a.toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }

    }while((xnum >= 3 || a == b || a == c || b == c || a == 0 ))

    var q = " "
    if(b > 0){
      if(a == 1){
        q = "x + "+b.toString+" = "+c.toString
      }else{
        q = a.toString + "x + "+b.toString+" = "+c.toString
      }
    }else{
      if(a == 1){
        q = "x "+b.toString+" = "+c.toString
      }else{
        q = a.toString + "x "+b.toString+" = "+c.toString
      }
    }
    
    ans_change(q,x,range*100)
  }
  //ax+b=cx+d
  def two(qnum:Int = 1,range:Int)={
    var a,b,c,d = 0
    var x = 0f
    var xnum=100
    var n = true
    do{

      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)

      x = (b-d)/(a-c).toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
    }while((a-c) == 0 || xnum >= 3 )

    var q = ""

    if(a  == 1 && c == 1){
      if(b > 0 && d > 0){
        q = "x + "+str(b) +" = "+"x + "+str(d)
      }else if(b > 0 && d < 0){
        q = "x + "+str(b) +" = "+"x "+str(d)
      }else if(b < 0 && d > 0){
        q = "x "+str(b) +" = "+"x + "+str(d)
      }else{
        q = "x "+str(b) +" = "+"x "+str(d)
      }
    }else if(a == 1 && c != 1){
      if(b > 0 && d > 0){
        q = "x + "+str(b) +" = "+str(c)+"x + "+str(d)
      }else if(b > 0 && d < 0){
        q = "x + "+str(b) +" = "+str(c)+"x "+str(d)
      }else if(b < 0 && d > 0){
        q = "x "+str(b) +" = "+str(c)+"x + "+str(d)
      }else{
        q = "x "+str(b) +" = "+str(c)+"x "+str(d)
      }
    }else if(a != 1 && c == 1){
      if(b > 0 && d > 0){
        q = str(a)+"x + "+str(b) +" = x + "+str(d)
      }else if(b > 0 && d < 0){
        q = str(a)+"x + "+str(b) +" = x "+str(d)
      }else if(b < 0 && d > 0){
        q = str(a)+"x "+str(b) +" = x+ "+str(d)
      }else{
        q = str(a)+"x "+str(b) +" = x "+str(d)
      }
    }else{
      if(b > 0 && d > 0){
        q = str(a)+"x + "+str(b)+" = "+str(c)+"x + "+str(d)
      }else if(b > 0 && d < 0){
        q = str(a)+"x + "+str(b) +" = "+str(c)+"x "+str(d)
      }else if(b < 0 && d > 0){
        q = str(a)+"x "+str(b) +" = "+str(c)+"x + "+str(d)
      }else{
        q = str(a)+"x "+str(b) +" = "+str(c)+"x "+str(d)
      }
    }

    ans_change(q,x,range*100)
  }
  //b/ax+c=d
  def three(qnum:Int = 1,range:Int)={

    var a,b,c,d = 0

    var x = 0f
    var xnum = 100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)

      x = (d-c)*b/a.toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
      if(gcd(a,b) != 1){
        xnum = 1234567
      }
      //|| a == b || a == c || b == c || a == d || d == c || d == b
    }while(xnum >= 3 ||b == 0 ||a == 0||a < 0||b == 1  )

    var q = ""

    if(b > 0 && c > 0){
      q =  frac(a,b)+"x + "+str(c)+" = "+str(d)
    }else if(b > 0 && c < 0){
      q =  frac(a,b)+"x "+str(c)+" = "+str(d)
    }else if(b < 0 && c > 0){
      q = frac(a,b)+"x + "+str(c)+" = "+str(d)
    }else{
      q = frac(a,b)+"x "+str(c)+" = "+str(d)
    }

    ans_change(q,x,range*100)
  }

  def three_dash(qnum:Int = 1,range:Int)={

    var a,b,c,d,e = 0

    var x = 0f
    var xnum = 100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)
      e = random(rand.nextInt(range)+1)

      x = (d-e*c)*b / (e*a).toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
      if(gcd(a,b) != 1){
        xnum = 1234567
      }
      //|| a == b || a == c || b == c || a == d || d == c || d == b
    }while(xnum >= 3 ||b == 0 ||a == 0 || a < 0 ||b == 1 || e == 0 || e ==  1)

    var q = ""

    if(b > 0 && c > 0){
      q =  frac(a,b)+"x + "+str(c)+" = "+frac(d,e)
    }else if(b > 0 && c < 0){
      q =  frac(a,b)+"x "+str(c)+" = "+frac(d,e)
    }else if(b < 0 && c > 0){
      q = frac(a,b)+"x + "+str(c)+" = "+frac(d,e)
    }else{
      q = frac(a,b)+"x "+str(c)+" = "+frac(d,e)
    }

    ans_change(q,x,range*100)
  }

  //b/ax+c = d/e
  def fore(qnum:Int = 1,range:Int)={

    var a,b,c,d,e = 0
    var x = 0f
    var xnum=100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)
      e = random(rand.nextInt(range)+1)

      x = (e.toFloat-c/d.toFloat)*b/a.toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
      if(gcd(a,b) != 1 || gcd(c,d) != 1){
        xnum = 1234567
      }
      //|| a == b || a == c || b == c || a == d || d == c || d == b
    }while(
      xnum >= 3 ||
      b == 0 ||
      a == 0 ||
      e == 0 ||
      d == 0 ||
      b == 1 ||
      d == 1 ||
      a == b ||
      c == d
    )

    var q = ""

    if(c < 0 || d < 0){
      q = frac(a,b)+"x "+frac(c,d)+" = "+str(e)
    }else {
      q = frac(a,b)+"x +"+frac(c,d)+" = "+str(e)
    }

    ans_change(q,x,range*100)
  }

   def fore_dash(qnum:Int = 1,range:Int)={

    var a,b,c,d,e,f = 0
    var x = 0f
    var xnum=100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)
      e = random(rand.nextInt(range)+1)
      f = random(rand.nextInt(range)+1)

      x = (d*e-c*f)*b/(d*f*a)
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
      if(gcd(a,b) != 1 || gcd(c,d) != 1){
        xnum = 1234567
      }
      //|| a == b || a == c || b == c || a == d || d == c || d == b
    }while(
      xnum >= 3 ||
        b == 0 ||
        a == 0 ||
        e == 0 ||
        d == 0 ||
        b == 1 ||
        d == 1 ||
        f == 0 || f == 1 ||
        a == b ||
        c == d
    )

    var q = ""

    if(c < 0 || d < 0){
      q = frac(a,b)+"x "+frac(c,d)+" = "+frac(e,f)
    }else {
      q = frac(a,b)+"x +"+frac(c,d)+" = "+frac(e,f)
    }
     ans_change(q,x,range*100)
   }

  def five(qnum:Int = 1,range:Int)={

    var a,b,c,d,e = 0
    var x = 0f
    var xnum=100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)
      e = random(rand.nextInt(range)+1)

      x = -b/c.toFloat / (a.toFloat - d/e.toFloat)
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
      if(gcd(b,c) != 1 || gcd(d,e) != 1){
        xnum = 1234567
      }

    }while(
      xnum >= 3 ||
      b == 0 ||
      a == 0 ||
      e == 0 ||
      d == 0 || d == e ||
      c == 1 ||
      e == 1 ||
      c == b
    )

    var q = ""
    if(a == 1){
      if(b < 0 || c < 0){
        q = "x"+frac(b,c)+"="+frac(d,e)+"x"
      }else {
        q = "x+"+frac(b,c)+"="+frac(d,e)+"x"
      }
    }else if (a == -1){
      if(b < 0 || c < 0){
        q = "-x"+frac(b,c)+"="+frac(d,e)+"x"
      }else {
        q = "-x+"+frac(b,c)+"="+frac(d,e)+"x"
      }
    }else{
      if(b < 0 || c < 0){
        q = str(a)+"x"+frac(b,c)+"="+frac(d,e)+"x"
      }else {
        q = str(a)+"x+"+frac(b,c)+"="+frac(d,e)+"x"
      }
    }
    ans_change(q,x,range*100)
  }

  def six(qnum:Int = 1,range:Int)={

    var a,b,c,d,e = 0
    var x = 0f
    var xnum=100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)
      e = random(rand.nextInt(range)+1)

      x = (d*b*e)/(b*c+a*d).toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
      if(gcd(a,b) != 1 || gcd(c,d) != 1){
        xnum = 1234567
      }

    }while(
      xnum >= 3 ||
        b*c+a*d == 0 ||
        b == 0 ||
        a == 0 ||
        e == 0 ||
        d == 0 ||
        c == 1 ||
        d == 1 ||
        b == a ||
        d == c
    )

    var q = ""
    if(c < 0 || d < 0){
      q = frac(a,b)+"x"+frac(c,d)+"x = "+str(e)
    }else{
      q = frac(a,b)+"x+"+frac(c,d)+"x = "+str(e)
    }
     ans_change(q,x,range*100)
  }

  def six_dash(qnum:Int = 1,range:Int)={

    var a,b,c,d,e,f = 0
    var x = 0f
    var xnum=100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)
      e = random(rand.nextInt(range)+1)
      f = random(rand.nextInt(range)+1)

      x = (e*b*d)/((b*c+a*d)*f).toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
      if(gcd(a,b) != 1 || gcd(c,d) != 1){
        xnum = 1234567
      }

    }while(
      xnum >= 3 ||
        b*c+a*d == 0 ||
        b == 0 ||
        a == 0 ||
        e == 0 ||
        d == 0 ||
        c == 1 ||
        f == 0 ||
        f == 1 ||
        d == 1 ||
        b == a ||
        d == c
    )

    var q = ""
    if(c < 0 || d < 0){
      q = frac(a,b)+"x"+frac(c,d)+"x = "+frac(e,f)
    }else{
      q = frac(a,b)+"x+"+frac(c,d)+"x = "+frac(e,f)
    }
    ans_change(q,x,range*100)
  }

  def seven(qnum:Int = 1,range:Int)={

    var a,b,c,d,e,f = 0
    var x = 0f
    var xnum=100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)
      d = random(rand.nextInt(range)+1)
      e = random(rand.nextInt(range)+1)
      f = random(rand.nextInt(range)+1)

      x = (c*e-b*f)/(a*f-c*d).toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
      if(gcd(a,b) != 1 || gcd(c,d) != 1){
        xnum = 1234567
      }

    }while(
      xnum >= 3 ||
        a*f-c*d == 0 ||
        a*b*c*d*e*f == 0 ||
        c == 1 ||
        f == 1
     )

    var q = ""
    var q1 = ""
    var q2 = ""
    if(a == 1){
      if(b > 0){
        if(c > 0){
          q += Sfrac(("x+"+str(b)),str(abs(c)),true)
        }else{
          q += Sfrac(("x+"+str(b)),str(abs(c)),false)
        }
      }else{
        if(c > 0){
          q += Sfrac(("x"+str(b)),str(abs(c)),true)
        }else{
          q += Sfrac(("x"+str(b)),str(abs(c)),false)
        }
      }
    }else if(a == -1){
      if(b > 0){
        if(c > 0){
          q += Sfrac(("-x+"+str(b)),str(abs(c)),true)
        }else{
          q += Sfrac(("-x+"+str(b)),str(abs(c)),false)
        }
      }else{
        if(c > 0){
          q += Sfrac(("-x"+str(b)),str(abs(c)),true)
        }else{
          q += Sfrac(("-x"+str(b)),str(abs(c)),false)
        }
      }
    }else{
      if(b > 0){
        if(c > 0){
          q += Sfrac((str(a)+"x+"+str(b)),str(abs(c)),true)
        }else{
          q += Sfrac((str(a)+"x+"+str(b)),str(abs(c)),false)
        }
      }else{
        if(c > 0){
          q += Sfrac((str(a)+"x"+str(b)),str(abs(c)),true)
        }else{
          q += Sfrac((str(a)+"x"+str(b)),str(abs(c)),false)
        }
      }
    }
    q += " = "

    if(d == 1){
      if(e > 0){
        if(f > 0){
          q += Sfrac(("x +"+str(e)),str(abs(f)),true)
        }else{
          q += Sfrac(("x +"+str(e)),str(abs(f)),false)
        }
      }else{
        if(f > 0){
          q += Sfrac(("x "+str(e)),str(abs(f)),true)
        }else{
          q += Sfrac(("x "+str(e)),str(abs(f)),false)
        }
      }
    }else if(d == -1){
      if(e > 0){
        if(f > 0){
          q += Sfrac(("-x +"+str(e)),str(abs(f)),true)
        }else{
          q += Sfrac(("-x +"+str(e)),str(abs(f)),false)
        }
      }else{
        if(f > 0){
          q += Sfrac(("-x "+str(e)),str(abs(f)),true)
        }else{
          q += Sfrac(("-x "+str(e)),str(abs(f)),false)
        }
      }
    }else{
      if(e > 0){
        if(f > 0){
          q += Sfrac((str(d)+"x +"+str(e)),str(abs(f)),true)
        }else{
          q += Sfrac((str(d)+"x +"+str(e)),str(abs(f)),false)
        }
      }else{
        if(e > 0){
          q += Sfrac((str(d)+"x "+str(e)),str(abs(f)),true)
        }else{
          q += Sfrac((str(d)+"x "+str(e)),str(abs(f)),false)
        }
      }
    }


      ans_change(q,x,range*100)
   
  }



  
  def frac_change2(xx:Double,range:Int)={

    val son = Range(1,range+1,1).toArray
    val mother = Range(1,range+1,1).toArray
    var A = ""
    var ans = ""
    var d = 10000d
    if(xx < 0){
        A = "-"
    }

    var x = abs(xx)
    for(i <- son;j <- mother){
      if(abs(i/j.toDouble - x) < 1e-2 && abs(i/j.toDouble - x) < d){
        if(gcd(i,j) == 1){
          ans = A + frac(i,j)
          d = abs(i/j.toDouble - x)
        }
      }
    }
    ans
  }

}
