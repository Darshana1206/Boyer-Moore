package new1

import scala.util.control.Breaks._
import scala.io.Source
import java.io.File
import java.io.{BufferedWriter, FileWriter, File, PrintWriter}

object boyer {
  def main(args:Array[String])
  {
     var s:String= (" ")
    var t:Char=s.charAt(0)
    
    var count:Int=0
    var j:Int=0
    var i:Int=0
    var n:Int=0
    
    var newArr = new Array[Char](100000000) 
    
   for(line <- Source.fromFile("pi.txt").getLines())
   {
     count+=1
     if(count>21)
     {
        var piArr= line.toCharArray()
        var m=piArr.length
        if(m>20)
        {
          for(i<-0 until m)
          {
            if(piArr(i)==t)
            {
              for(j<-i until (m-1))
              {
                piArr(j)=piArr(j+1)
              }
            }
          }
          for(i<-0 until 50)
          {
            newArr(i+n)=piArr(i)  
          }
          n=n+50
        }  
     } 
   }
     var p="971120"
     var pat = p.toCharArray()
    search(newArr,pat)  
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
         val fw=new FileWriter("BoyerMoore.txt",true);
         fw.write("pattern found in index : "+s+"\n")   
         fw.close()
        
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