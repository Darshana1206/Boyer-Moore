package new1

object boyer {
  def main(args:Array[String])
  {
     var t= "ABCDFGTRABC"
     var txt = t.toCharArray()
     
     var p="ABC"
     var pat=p.toCharArray()
     
     search(txt,pat)
  }
  def max(a:Int,b:Int):Int=
  {
    if(a>b)
      return a
      
    else
      return b  
  }
  def badCharactor(mystr:Array[Char],p:Int,badChar:Array[Int])
  {
    var i=0
    val n=256 // Number of chars
    
   // var badChar=new Array[Int](n)
    for(i<-0 until n)
    {
      badChar(i)=(-1)
    }
    for(i<-0 until p)
    {
      badChar( mystr(i).toInt)=i
    }
  }
  def search(txt:Array[Char],pat:Array[Char])
  {
    var a= txt.length
    var b=pat.length
    val n=256
    var badChar=new Array[Int](n)
    
    badCharactor(pat,b,badChar)
    var s=0
    
    while(s<=(a-b))
    {
      var j=b-1
      
      while (j>=0 && pat.charAt(j)==txt.charAt(s+j))
      {
        j=j-1
      }
      if(j<0)
      {
        println(s)
        
        if((s+b)<a)
        {
          s=s+ b- badChar(txt.charAt(s+b))
        }else
        {
          s=s+1
        }
      }else
        s+=max(1,(j-badChar(txt.charAt(s+j))))
    }
  }  
}