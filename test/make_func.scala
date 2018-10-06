import math._
object  make_func {
  val rand = new scala.util.Random(0)
  def frac (a:Int,b:Int)={
    var q = ""
    if(a > 0 && b > 0){
      q ="""\flac{"""+str(a)+"}{"+str(b)+"}"
    }else if(a < 0 || b < 0){
      q ="""-\flac{"""+str(abs(a))+"}{"+str(abs(b))+"}"
    }
    q
  }
  def main(args: Array[String]): Unit = {
    for(one <- 0 until 10){
      println("make "+one)
      val pathName = "test/question"+str(one)+".txt"
      val writer = new java.io.PrintWriter(pathName)

      var ys = ""
      
      //ys += one(1,10)._1+"\n"
     //ys += one(1,10)._1+"\n"
      ys += two(1,10)._1+"\n"
      ys += two(1,10)._1+"\n"
      ys += three(1,10)._1+"\n"
      ys += three(1,10)._1+"\n"
      ys += fore(1,10)._1+"\n"
      ys += fore(1,10)._1+"\n"
      ys += five(1,10)._1+"\n"
      ys += five(1,10)._1+"\n"

      writer.write(ys)
      writer.close()

      val pathName2 = "test/ans"+str(one)+".txt"
      val writer2 = new java.io.PrintWriter(pathName2)

      var as = ""
      
   //   as += one(1,10)._2+"\n"
    //  as += one(1,10)._2+"\n"
      as += two(1,10)._2+"\n"
      as += two(1,10)._2+"\n"
      as += three(1,10)._2+"\n"
      as += three(1,10)._2+"\n"
      as += fore(1,10)._2+"\n"
      as += fore(1,10)._2+"\n"
      as += five(1,10)._2+"\n"
      as += five(1,10)._2+"\n"

      writer2.write(as)
      writer2.close()
  
    }
  }
  def str (a:Int) = a.toString
  def str (a:Double) = a.toString
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def random(a:Int) = if(rand.nextInt(2)==0) -1 * a else a
  //ax+b = c
  def one(qnum:Int = 1,range:Int)={
    var a,b,c = 0
    var x = 0f
    var xnum=100
    var n = true
    do{
      a = random(rand.nextInt(range)+1)
      b = random(rand.nextInt(range)+1)
      c = random(rand.nextInt(range)+1)

      x = (b-c)/a.toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }

    }while((xnum >= 3 || a == b || a == c || b == c || a == 0 ))

    var q = ""
    if(b > 0){
      if(a == 1){
        q = "x+"+b.toString+"="+c.toString
      }else{
        q = a.toString + "x+"+b.toString+"="+c.toString
      }
    }else{
      if(a == 1){
        q = "x"+b.toString+"="+c.toString
      }else{
        q = a.toString + "x"+b.toString+"="+c.toString
      }
    }

    val shou = str(x).split('.').size
    var xx = ""
    println(x,shou)
    if(x % 1.0 == 0){
      xx = str(x.toInt)
     
    }else{
      xx = frac_change2(x,range)
    }
    (q,xx)
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
      /*
      a = rand.nextInt(range)+1
      b = rand.nextInt(range)+1
      c = rand.nextInt(range)+1
      d = rand.nextInt(range)+1
      */
      x = (b-d)/(a-c).toFloat
      var xn = x.toString.split('.').toArray
      if(xn.size == 2){
        xnum = xn(1).size
      }
    }while((a-c) == 0 || xnum >= 3 )

    var q = ""

    if(a  == 1 && c == 1){
      if(b > 0 && d > 0){
        q = "x+"+str(b) +"="+"x+"+str(d)
      }else if(b > 0 && d < 0){
        q = "x+"+str(b) +"="+"x"+str(d)
      }else if(b < 0 && d > 0){
        q = "x"+str(b) +"="+"x+"+str(d)
      }else{
        q = "x"+str(b) +"="+"x"+str(d)
      }
    }else if(a == 1 && c != 1){
      if(b > 0 && d > 0){
        q = "x+"+str(b) +"="+str(c)+"x+"+str(d)
      }else if(b > 0 && d < 0){
        q = "x+"+str(b) +"="+str(c)+"x"+str(d)
      }else if(b < 0 && d > 0){
        q = "x"+str(b) +"="+str(c)+"x+"+str(d)
      }else{
        q = "x"+str(b) +"="+str(c)+"x"+str(d)
      }
    }else if(a != 1 && c == 1){
      if(b > 0 && d > 0){
        q = str(a)+"x+"+str(b) +"="+"x+"+str(d)
      }else if(b > 0 && d < 0){
        q = str(a)+"x+"+str(b) +"="+"x"+str(d)
      }else if(b < 0 && d > 0){
        q = str(a)+"x"+str(b) +"="+"x+"+str(d)
      }else{
        q = str(a)+"x"+str(b) +"="+"x"+str(d)
      }
    }else{
       if(b > 0 && d > 0){
        q = str(a)+"x+"+str(b)+"="+str(c)+"x+"+str(d)
      }else if(b > 0 && d < 0){
        q = str(a)+"x+"+str(b) +"="+str(c)+"x"+str(d)
      }else if(b < 0 && d > 0){
        q = str(a)+"x"+str(b) +"="+str(c)+"x+"+str(d)
      }else{
        q = str(a)+"x"+str(b) +"="+str(c)+"x"+str(d)
      }
    }

    val shou = str(x).split('.').size
    var xx = ""
    if(shou > 1){
      xx = frac_change2(x,range)
    }else{
      xx = str(x)
    }
    (q,xx)
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

      x = (d-c)*a/b.toFloat
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
     q =  frac(a,b)+"x+"+str(c)+"="+str(d)
    }else if(b > 0 && c < 0){
      q =  frac(a,b)+"x"+str(c)+"="+str(d)
    }else if(b < 0 && c > 0){
      q = frac(a,b)+"x+"+str(c)+"="+str(d)
    }else{
      q = frac(a,b)+"x"+str(c)+"="+str(d)
    }
    
    val shou = str(x).split('.').size
    var xx = ""
    if(shou > 1){
      xx = frac_change2(x,range)
    }else{
      xx = str(x)
    }
    (q,xx)
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

      x = (d/e.toFloat-c.toFloat)*a/b.toFloat
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
      q = frac(a,b)+"x"+frac(c,d)+"="+str(e)
    }else {
      q = frac(a,b)+"x+"+frac(c,d)+"="+str(e)
    }
    val shou = str(x).split('.').size
    var xx = ""
    if(shou > 1){
      xx = frac_change2(x,range)
    }else{
      xx = str(x)
    }
    (q,xx)
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

      x = (d/e.toFloat-c.toFloat)*a/b.toFloat
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
        d == 0 ||
        c == 1 ||
        e == 1 ||
        d == e ||
        c == b
    )

    var q = ""
      if(a == 1){
        if(b < 0 || c < 0){
          q = "x"+frac(b,c)+"="+frac(d,e)+"x"
        }else {
          q = "x+"+frac(b,c)+"="+frac(d,e)+"x"
        }
      }else{
        if(b < 0 || c < 0){
          q = str(a)+"x"+frac(b,c)+"="+frac(d,e)+"x"
        }else {
          q = str(a)+"x+"+frac(b,c)+"="+frac(d,e)+"x"
        }
      }
    val shou = str(x).split('.').size
    var xx = ""
    if(shou > 1){
      xx = frac_change2(x,range)
    }else{
      xx = str(x)
    }
    (q,xx)
  }

  def frac_change(x:Double)={
    var q = 0

    val n = str(x).split('.')
    var num =100000f
    var mother = num //pow(10,num)
    var son = x * mother
    println(x,num,son,mother)
    val g =gcd(son.toInt,mother.toInt)
    println(son,mother,g)
    son = son / g
    mother = mother / g
    println(son,mother,g)

    frac(son.toInt,mother.toInt)
  }

  def frac_change2(x:Double,range:Int)={
    val son = Range(1,range+1,1).toArray
    val mother = Range(1,range+1,1).toArray
    var ans =""
    var d = 10000d
    for(i <- son;j <- mother){
      if(abs(i/j.toDouble - x) < 1e-2 && abs(i/j.toDouble - x) < d){
        if(gcd(i,j) == 1){
          ans =frac(i,j)
          d = abs(i/j - x)
        }
      }
    }
    ans
  }

}
