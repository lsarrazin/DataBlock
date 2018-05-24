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
      
      dn.dump
      
    }
    
    "and give them back" in {
      
      val val1: Any = dn.get("Val1")
      println("val1 = " + val1)
      val val2: Any = dn.get("Val2")
      println("val2 = " + val2)
      
      assert(dn.get("Val1") === Some("Hello, world!"))
      assert(dn.get("Val2") === Some(1234567890))
    }
  }
  
  "A DataNode" - { 
    val dn = DataNode.newNode
    dn.put("Val1", "Hello, world!")
    dn.put("Val2", 1234567890)
    dn.dump
    
    "should contain inner nodes" in {
      val sdn = dn.newNode
      sdn.put("SVal1", true)
      sdn.put("SVal2", "Underground")
      dn.put("Val3", sdn)
      dn.dump
    
      assert(dn.get("Val1") === Some("Hello, world!"))
      assert(dn.get("Val2") === Some(1234567890))
      assert(sdn.get("SVal1") === Some(true))
      assert(sdn.get("SVal2") === Some("Underground"))
      assert(dn.get("Val3/SVal1") === Some(true))
      assert(dn.get("Val3/SVal2") === Some("Underground"))
    }

    "should contain more inner nodes" in {
      val sdn = dn.newNode
      sdn.put("EVal1", 123.456)
      sdn.put("EVal2", "SecondSet")
      dn.put("Val4", sdn)
      dn.dump
    
      assert(dn.get("Val3/SVal1") === Some(true))
      assert(dn.get("Val3/SVal2") === Some("Underground"))
      assert(dn.get("Val4/EVal1") === Some(123.456))
      assert(dn.get("Val4/EVal2") === Some("SecondSet"))
    }
  } 
  
  // TODO: Add more cases
  // - Overwrite value
  // - Prune & graft
  // - Delete values
  // - Clone
}