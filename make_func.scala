import math._
object  make_func {
  val rand = new scala.util.Random(0)
  def main(args: Array[String]): Unit = {
  }
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  //def gcd(a: Float, b: Float): Float = if (b == 0f) a else gcd(b, a % b)
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
        q = "x+"+b.toString+" = "+c.toString
      }else{
        q = a.toString + "x+"+b.toString+" = "+c.toString
      }
    }else{
      if(a == 1){
        q = "x"+b.toString+" = "+c.toString
      }else{
        q = a.toString + "x"+b.toString+" = "+c.toString
      }
    }


    (q,x)
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
      // || a == b || a == c || b == c || a == d || d == c || d == b
      println(xnum,a-c,x)
    }while((a-c) == 0 || xnum >= 3 )

    var q = ""
    if(b > 0 && d > 0){
      if(a == 1 && c == 1) {
        q = "x+"+b.toString+" = "+c.toString+"x+"+d.toString
      }else if(a == 1){
        q = "x+"+b.toString+" = "+"x+"+d.toString
      }else if(c == 1){
        q = a.toString+"x+"+b.toString+" = "+"x+"+d.toString
      }else{
        q = a.toString + "x+"+b.toString+" = "+c.toString +"x+"+d.toString
      }
    }else if(b > 0){
      if(a == 1 && c == 1) {
        q = "x+"+b.toString+" = "+c.toString+"x"+d.toString
      }else if(a == 1){
        q = "x+"+b.toString+" = "+"x"+d.toString
      }else if(c == 1){
        q = a.toString+"x+"+b.toString+" = "+"x"+d.toString
      }else{
        q = a.toString + "x+"+b.toString+" = "+c.toString +"x"+d.toString
      }
    }else{
      if(a == 1 && c == 1) {
        q = "x"+b.toString+" = "+c.toString+"x"+d.toString
      }else if(a == 1){
        q = "x"+b.toString+" = "+"x"+d.toString
      }else if(c == 1){
        q = a.toString+"x"+b.toString+" = "+"x"+d.toString
      }else{
        q = a.toString + "x"+b.toString+" = "+c.toString +"x"+d.toString
      }

    }


    q
  }
  //b/ax+c=d
  def three(qnum:Int = 1,range:Int)={

    var a,b,c,d = 0

    var x = 0f
    var xnum=100
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
    }while((xnum >= 3 ||b == 0 ||a == 0 ))

    var q = ""
    if(a > 0 && b > 0 && c > 0){
      q =  "flac{"+b.toString+"}"+"{"+a.toString+"}"+"x+"+c.toString +"="+d.toString
    }else if((a < 0 || b < 0) && (c > 0)){
      q =  "-flac{"+b.toString+"}"+"{"+a.toString+"}"+"x+"+c.toString +"="+d.toString
    }else if((a < 0 || b < 0) && (c < 0)){
      q =  "-flac{"+b.toString+"}"+"{"+a.toString+"}"+"x"+c.toString +"="+d.toString
    }else{
      println("non match")
    }

    (q,frac_change(x,range))
  }
  //b/ax+c = d/e
  def fore(qnum:Int = 1,range:Int)={

    var a,b,c,d,e = 0f
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
      if(gcd(a,b) != 1){
        xnum = 1234567
      }
      //|| a == b || a == c || b == c || a == d || d == c || d == b
    }while((xnum >= 3 ||b == 0 ||a == 0|| e == 0 || d == 0))

    var q = "1"
    println(a,b,c,d,e)
    if(a > 0 && b > 0 && c > 0 && d > 0 && e > 0){
      a = abs(a).toInt
      b = abs(b).toInt
      c = abs(c).toInt
      d = abs(d).toInt
      e = abs(e).toInt
      q =  "flac{"+b.toString+"}"+"{"+a.toString+"}"+"x+"+c.toString +"="+"flac{"+d.toString+"}"+"{"+e.toString+"}"
    }else if((a < 0 || b < 0) && c > 0 && (d > 0 || e > 0)){
      a = abs(a).toInt
      b = abs(b).toInt
      c = abs(c).toInt
      d = abs(d).toInt
      e = abs(e).toInt
      q =  "-flac{"+b.toString+"}"+"{"+a.toString+"}"+"x+"+c.toString +"="+"flac{"+d.toString+"}"+"{"+e.toString+"}"
    }else if((a < 0 || b < 0) && c > 0 && (d < 0 || e < 0)){
      a = abs(a).toInt
      b = abs(b).toInt
      c = abs(c).toInt
      d = abs(d).toInt
      e = abs(e).toInt
      q =  "-flac{"+b.toString+"}"+"{"+a.toString+"}"+"x+"+c.toString +"="+"-flac{"+d.toString+"}"+"{"+e.toString+"}"
    }else if(a > 0 && b > 0 && c < 0 && d > 0 && e > 0){
      a = abs(a).toInt
      b = abs(b).toInt
      c = abs(c).toInt
      d = abs(d).toInt
      e = abs(e).toInt
      q =  "flac{"+b.toString+"}"+"{"+a.toString+"}"+"x"+c.toString +"="+"flac{"+d.toString+"}"+"{"+e.toString+"}"
    }else if((a < 0 || b < 0) && c < 0 && (d > 0 || e > 0)){
      a = abs(a).toInt
      b = abs(b).toInt
      c = abs(c).toInt
      d = abs(d).toInt
      e = abs(e).toInt
      q =  "-flac{"+b.toString+"}"+"{"+a.toString+"}"+"x"+c.toString +"="+"flac{"+d.toString+"}"+"{"+e.toString+"}"
    }else if((a < 0 || b < 0) && c < 0 && (d < 0 || e < 0)){
      a = abs(a).toInt
      b = abs(b).toInt
      c = abs(c).toInt
      d = abs(d).toInt
      e = abs(e).toInt
      q =  "-flac{"+b.toString+"}"+"{"+a.toString+"}"+"x+"+c.toString +"="+"-flac{"+d.toString+"}"+"{"+e.toString+"}"
    }else{
      println(a,b,c,d,e)
      println("non match")
    }
    println(a,b,c,d,e)
    (q,frac_change(x,range))
  }

  def frac_change(x:Float,range:Int)={
    var son = Range(0,range,1).toArray
    var mother = Range(0,range,1).toArray
    var ans = (0,0)
    var r = true
    for(s <- son;m <- mother){
      if(x == s/m.toFloat && r ){
        ans = (s,m)
        println(ans)
        r = false
      }
    }
    var q = "frac{"+ans._1+"}{"+ans._2+"}"

    q
  }

}
