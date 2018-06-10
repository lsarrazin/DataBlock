package org.lsa.scala.data

import org.scalatest._
import org.scalactic.source.Position.apply

class DataNodeSpec extends FreeSpec {
  
  "An empty DataNode" - {
    val dn = DataNode.newNode

    "should contain nothing but root node" in {
      assert(dn.length === 1)
      assert(!dn.isEmpty)
    }
  }

  "An empty DataNode" - {
    val dn = DataNode.newNode
    
    "should accept values" in {
      println(dn.put("Val1", "Hello, world!"))
      println(dn.put("Val2", 1234567890))
    }
    
    "and give them back" in {
      val val1: Any = dn.get("Val1")
      println("val1 = " + val1)
      val val2: Any = dn.get("Val2")
      println("val2 = " + val2)
      
      assert(dn.get("Val1").contains("Hello, world!"))
      assert(dn.get("Val2").contains(1234567890))
    }
  }
  
  "A DataNode" - { 
    val dn = DataNode.newNode
    dn.put("Val1", "Hello, world!")
    dn.put("Val2", 1234567890)
    
    "should contain inner nodes" in {
      val sdn = dn.newNode
      sdn.put("SVal1", true)
      sdn.put("SVal2", "Underground")
      dn.put("Val3", sdn)
    
      assert(dn.get("Val1").contains("Hello, world!"))
      assert(dn.get("Val2").contains(1234567890))
      assert(sdn.get("SVal1").contains(true))
      assert(sdn.get("SVal2").contains("Underground"))
      assert(dn.get("Val3/SVal1").contains(true))
      assert(dn.get("Val3/SVal2").contains("Underground"))
    }

    "should contain more inner nodes" in {
      val sdn = dn.newNode
      sdn.put("EVal1", 123.456)
      sdn.put("EVal2", "SecondSet")
      dn.put("Val4", sdn)
    
      assert(dn.get("Val3/SVal1").contains(true))
      assert(dn.get("Val3/SVal2").contains("Underground"))
      assert(dn.get("Val4/EVal1").contains(123.456))
      assert(dn.get("Val4/EVal2").contains("SecondSet"))
    }
    
    "should allow multiple values with the same name" in {
      dn.put("List", "Item1")
      dn.put("List", "Item2")
      dn.put("List", 123456)
      dn.dump
      assert(dn.get("List").contains(List("Item1", "Item2", 123456)))
      
      dn.put("List2", List(127.toByte, true, 12345678, "Hello"))
      dn.dump
      assert(dn.get("List2").contains(List(127.toByte, true, 12345678, "Hello")))

      dn.put("List2", List("World", 123.456, false))
      dn.dump
      assert(dn.get("List2").contains(List(127.toByte, true, 12345678, "Hello", "World", 123.456, false)))
      
    }
  } 
  
  // TODO: Add more cases
  // - Overwrite value
  // - Prune & graft
  // - Delete values
  // - Clone
}