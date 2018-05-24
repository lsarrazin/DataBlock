package org.lsa.scala.data

import org.scalatest._
import org.scalactic.source.Position.apply

class MemoryBlockSpec extends FreeSpec {

  def bytePrinter(b: Byte): String = b.formatted("%02x")

  "An empty MemoryBlock" - {
    val mb = MemoryBlock.newBlock(32)

    "should carry no byte" in {
      assert(mb.asString(bytePrinter) === "")
    }

    "should not return a value" in {
      assert(mb.peek(0) == None)
    }
  }

  "A MemoryBlock containing two booleans" - {
    val mb = MemoryBlock.newBlock(32)
    mb.poke(true)
    mb.poke(false)

    "should carry four bytes" in {
      assert(mb.length == 4)
    }

    "should encode correctly" in {
      assert(mb.asString(bytePrinter) === "01.01.01.00")
    }

    "should give back two booleans" in {
      assert(mb.peek(0) == Some(true))
      assert(mb.peek(2) == Some(false))
    }
  }

  "A MemoryBlock containing a byte" - {
    val mb = MemoryBlock.newBlock(32)
    mb.poke(32.toByte)

    "should carry 2 bytes" in {
      assert(mb.length == 2)
    }

    "should encode correctly" in {
      assert(mb.asString(bytePrinter) === "02.20")
    }

    "should give back a byte" in {
      assert(mb.peek(0, None) === 32.toByte)
    }
  }

  "A MemoryBlock containing a double value" - {
    val mb = MemoryBlock.newBlock(32)
    mb.poke(8192.12)

    "should carry 9 bytes" in {
      assert(mb.length == 9)
    }

    "should encode correctly" in {
      assert(mb.asString(bytePrinter) === "03.40.c0.00.0f.5c.28.f5.c3")
    }

    "should give back a double" in {
      assert(mb.peek(0, None) === 8192.12)
    }
  }

  "A MemoryBlock containing a short int" - {
    val mb = MemoryBlock.newBlock(32)
    mb.poke(8192.toShort)

    "should carry 3 bytes" in {
      assert(mb.length == 3)
    }

    "should encode correctly" in {
      assert(mb.asString(bytePrinter) === "04.20.00")
    }

    "should give back a short int" in {
      assert(mb.peek(0, None) === 8192.toShort)
    }
  }

  "A MemoryBlock containing an int" - {
    val mb = MemoryBlock.newBlock(32)
    mb.poke(8192)

    "should carry 5 bytes" in {
      assert(mb.length == 5)
    }

    "should encode correctly" in {
      assert(mb.asString(bytePrinter) === "05.00.00.20.00")
    }

    "should give back an int" in {
      val rs = mb.peek(0, "")
      assert(mb.peek(0, None) === 8192)
    }
  }

  "A MemoryBlock containing a long int" - {
    val mb = MemoryBlock.newBlock(32)
    mb.poke(8192.toLong)

    "should carry 9 bytes" in {
      assert(mb.length == 9)
    }

    "should encode correctly" in {
      assert(mb.asString(bytePrinter) === "06.00.00.00.00.00.00.20.00")
    }

    "should give back a long int" in {
      assert(mb.peek(0, None) === 8192.toLong)
    }
  }

  "A MemoryBlock with pageSize 16" - {
    val mb = MemoryBlock.newBlock(16)

    "containing the unicode string \"Hello, world\"" - {
      mb.poke("Hello, world!")

      "should carry 28 bytes" in {
        assert(mb.length == 28)
      }

      "should encode correctly" in {
        assert(mb.asString(bytePrinter) === "17.1a.00.48.00.65.00.6c.00.6c.00.6f.00.2c.00.20|00.77.00.6f.00.72.00.6c.00.64.00.21")
      }

      "should give back the same string" in {
        assert(mb.peek(0, "") === "Hello, world!")
      }
    }

  }

  "A MemoryBlock with pageSize 16" - {
    val mb = MemoryBlock.newBlock(16)

    "now containing a longer string \"abc\" * 90" - {
      mb.poke("abc" * 90)

      "should carry 543 bytes" in {
        assert(mb.length == 543)
      }

      "should encode correctly" in {
        assert(mb.asString(bytePrinter).startsWith("27.02.1c.00.61.00.62.00.63"))
      }

      "should give back the same string" in {
        assert(mb.peek(0, "") === "abc" * 90)
      }
    }
  }

  "A MemoryBlock with many values" - {
    val mb = MemoryBlock.newBlock(16)
    val t0 = mb.getFreeOffset
    mb.poke(true)
    val t1 = mb.getFreeOffset
    mb.poke(128.toByte)
    val t2 = mb.getFreeOffset
    mb.poke(8192.toShort)
    val t3 = mb.getFreeOffset
    mb.poke(12345678)
    val t4 = mb.getFreeOffset
    mb.poke(Long.MaxValue / 2)
    val t5 = mb.getFreeOffset
    mb.poke("12345678")
    val t6 = mb.getFreeOffset
    mb.poke("ABC" * 5)
    val t7 = mb.getFreeOffset

    "should be of correct length" in {
      assert(mb.length == 71)
    }

    "must recall written values" in {
      assert(mb.peek(t0, None) === true)
      assert(mb.peek(t1, None) === 128.toByte)
      assert(mb.peek(t2, None) === 8192.toShort)
      assert(mb.peek(t3, None) === 12345678)
      assert(mb.peek(t4, None) === Long.MaxValue / 2)
      assert(mb.peek(t5, None) === "12345678")
      assert(mb.peek(t6, None) === "ABC" * 5)
    }

    "but no more" in {
      assert(mb.peek(t7) === None)
    }

  }

}