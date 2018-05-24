package org.lsa.scala.data

import org.scalatest._
import org.scalactic.source.Position.apply

class HashArraySpec extends FreeSpec {
  
  "An empty HashArray" - {
    val ps: Int = 256
    val ha: HashArray = new HashArray(ps, 4096)
    
    "should contain void elements" in {
      assert(ha.length == ps)
    }
    
    "should have an initial load factor of 0.0" in {
      assert(ha.getLoadFactor == 0.0)
    }
    
    "should hash consistently" in {
      val s1: String = "Value1"
      val s2: String = "Value2"
      val s3: String = "Value3"
      
      assert(ha.hashString(s1) == ha.hashString(s1))
      assert(ha.hashString(s2) == ha.hashString(s2))
      assert(ha.hashString(s3) == ha.hashString(s3))

      assert(ha.hashString(s1) != ha.hashString(s2))
      assert(ha.hashString(s2) != ha.hashString(s3))
      assert(ha.hashString(s1) != ha.hashString(s3))
    }
  }
  
  "A constrained HashArray(8)" - {
    val ht: HashArray = new HashArray(8, 4096)
    
    "should hash 23 different words using 8 buckets" in {
      val wl: String = "The heap memory is where the JVM stores runtime data represented by allocated instances Sequences are special cases of iterable collections of class Iterable"
      wl.split(" ").map(ht.put)
    }
    
    "with a length of 23" in {
      assert(ht.length == 23)
    }

    "that remains identical when inserting duplicates" in {
      ht.put("runtime")
      ht.put("by")
      assert(ht.length == 23)
    }

    "and end with a load factor of 1.0" in {
      assert(ht.getLoadFactor == 1.0)
    }
    
  }
  
  "An HashArray with initial values" - {
    val htb: HashArray = new HashArray(4, 4096)
    val wlb: String = "The heap memory is where the JVM stores runtime data represented by allocated instances Sequences are special cases of iterable collections of class Iterable"
    wlb.split(" ").map(htb.put)
    
    "should find inserted values" in {
      wlb.split(" ").foreach { w: String =>
        assert(htb.fetch(w) >= 0)
      }
    }
    
    "but not unknown values" in {
      assert(htb.fetch("memories") == -1)
      assert(htb.fetch("sequences") == -1)
    }
  }
  
  //TOOD: Test get method
}