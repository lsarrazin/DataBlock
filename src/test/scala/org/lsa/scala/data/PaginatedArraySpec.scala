package org.lsa.scala.data

import org.scalatest._
import org.scalactic.source.Position.apply

class PaginatedArraySpec extends FreeSpec {

  "An empty paginated array with pageSize 16" - {
    val epa = new PaginatedArray[Byte](16)

    "should have a length of 0" in {
      assert(epa.length === 0)
    }

    "should have a size of 16" in {
      assert(epa.size === 16)
    }
  }

  "A byte paginated array with pageSize 8" - {
    val bpa = new PaginatedArray[Byte](8)

    "with 12 bytes inserted one by one" - {
      (1 to 12).foreach { i: Int => bpa.writeElement(i.toByte) }

      "should have a length of 12" in {
        assert(bpa.length === 12)
      }

      "and a size of 16" in {
        assert(bpa.size === 16)
      }

      "and contain inserted bytes" in {
        val content: Array[Byte] = bpa.readElements(0, 12).getOrElse(Array.emptyByteArray)
        assert(content === (1 to 12).map { _.toByte })
      }

      "even read one by one" in {
        for (i <- 1 to 12)
          assert(i.toByte == bpa.readElement(i - 1).getOrElse(-1))
      }
    }
  }
  
  "A char paginated array" - {
    val bpa = new PaginatedArray[Char](8)
    
    "with a string inserted" - {
      "abcdefghijklmnop".map(bpa.writeElement)
      
      "should contain the inserted string" in {
        val content: String = String.valueOf(bpa.readElements(0, 16).get)
        assert(content == "abcdefghijklmnop")
      }
     
      "and should allow updates" in {
        (0 until 16 by 2).foreach { i =>
          bpa.updateElement(i, bpa.readElement(i).get.toUpper)
        }
        val content: String = String.valueOf(bpa.readElements(0, 16).get)
        assert(content == "AbCdEfGhIjKlMnOp")
      }
    }
  }
  
  "Any paginated array" - {
    val bpa =new PaginatedArray[Int](8)
    
    "should forbid incorrect offset" in {
      assert(bpa.seek(-1) == None)
      assert(bpa.seek(1234567890) == None)
    }
  }

  "Any paginated array" - {
    val bpa =new PaginatedArray[Int](8)
    
    "should insert values in order" in {
      val o1: Int = bpa.insertElement(123)
      val o2: Int = bpa.insertElement(456)
      assert(o1 < o2)
    }
    
    "and return element offset on insert" in {
      val o3: Int = bpa.insertElement(789)
      val o4: Int = bpa.insertElement(0)
      
      assert(bpa.readElement(o3).get == 789)
      assert(bpa.readElement(o4).get == 0)
    }
  }

}