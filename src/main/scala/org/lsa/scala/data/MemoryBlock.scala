package org.lsa.scala.data

import scala.annotation.tailrec
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

trait Serializer {

  def encodingLength(s: Int): Byte
  def getTypeLength(code: Byte): Int

  def anyToCode(v: Any): (Byte, Byte)
  def anyToCode(v: Any, bal: Int): Array[Byte]

  def anyToBytes(v: Any): Array[Byte]
  def bytesToAny(t: Byte, v: Array[Byte]): Any

  def intListToBytes(l: List[Int]): Array[Byte]
  def bytesToIntList(v: Array[Byte]): List[Int]

  final val UNKNOWN: Byte = 0
  final val BOOLEAN: Byte = 1
  final val BYTE: Byte = 2
  final val DOUBLE: Byte = 3
  final val SHORT: Byte = 4
  final val INT: Byte = 5
  final val LONG: Byte = 6
  final val STRING: Byte = 7
  final val LIST: Byte = 8
}

object BasicSerializer extends Serializer {

  def encodingLength(s: Int): Byte = {
    if (s < (1 << 8)) 1
    else if (s < (1 << 16)) 2
    else if (s < (1 << 24)) 3
    else 4
  }

  def getTypeLength(code: Byte): Int = code match {
    case BOOLEAN => 1
    case BYTE    => 1
    case DOUBLE  => 8
    case SHORT   => 2
    case INT     => 4
    case LONG    => 8
    case _       => 0
  }

  def anyToBytes(v: Any): Array[Byte] = v match {
    case b: Boolean => {
      val a = new Array[Byte](1)
      a(0) = if (b) 1 else 0
      a
    }
    case b: Byte => {
      val a = new Array[Byte](1)
      a(0) = b
      a
    }
    case d: Double => ByteBuffer.allocate(8).putDouble(d).array
    case s: Short  => ByteBuffer.allocate(2).putShort(s).array
    case i: Int    => ByteBuffer.allocate(4).putInt(i).array
    case l: Long   => ByteBuffer.allocate(8).putLong(l).array
    case s: String => {
      val bb = ByteBuffer.allocate(s.length() * 2)
      s.foreach(bb.putChar)
      bb.array
    }
    case _ => new Array[Byte](0)
  }

  def intListToBytes(l: List[Int]): Array[Byte] = {
    val len = l.length
    val bb = ByteBuffer.allocate(4 * len)
    for (i <- 0 until len) bb.putInt(i * 4, l(i))
    bb.array
  }

  def bytesToIntList(v: Array[Byte]): List[Int] = {
    val l: Int = v.length / 4
    (for (i <- 0 until l) yield {
      ByteBuffer.wrap(v, 4 * i, 4).asIntBuffer().get
    }).toList
  }

  def bytesToAny(t: Byte, v: Array[Byte]): Any = t match {
    case BOOLEAN => if (v(0) == 1) true else false
    case BYTE    => v(0)
    case DOUBLE  => ByteBuffer.wrap(v).asDoubleBuffer.get
    case SHORT   => ByteBuffer.wrap(v).asShortBuffer.get
    case INT     => ByteBuffer.wrap(v).asIntBuffer.get
    case LONG    => ByteBuffer.wrap(v).asLongBuffer.get
    case STRING  => new String(v, StandardCharsets.UTF_16)
    case _       => None
  }

  def anyToCode(v: Any): (Byte, Byte) = v match {
    case _: Boolean => (BOOLEAN, 0)
    case _: Byte    => (BYTE, 0)
    case _: Double  => (DOUBLE, 0)
    case _: Short   => (SHORT, 0)
    case _: Int     => (INT, 0)
    case _: Long    => (LONG, 0)
    case s: String  => (STRING, encodingLength(s.length))
    case l: List[_] => (LIST, encodingLength(l.length))
    case _          => (0, 0)
  }

  def anyToCode(v: Any, bal: Int): Array[Byte] = {
    val (code, len) = anyToCode(v)

    val bac = new Array[Byte](len + 1)
    bac(0) = ((len << 4) | code).toByte

    if (len > 0) {
      for (z <- 1 to len) {
        bac(1 + len - z) = ((bal / (1 << (8 * (z - 1)))) % 256).toByte
      }
    }

    bac
  }

}

object MemoryBlock {

  def defaultPageSize: Int = 2048

  def newBlock(pageSize: Int = defaultPageSize) = new MemoryBlock(pageSize)(BasicSerializer)

  def bytesToHex(bytes: Array[Byte], sep: String = ""): String = bytes.map("%02x".format(_)).mkString(sep)
  def bytesToHex(bytes: Array[Byte], os: Int, l: Int, sep: String): String = {
    bytes.slice(os, os + l).map("%02x".format(_)).mkString(sep)
  }

}

final class MemoryBlock(pageSize: Int = MemoryBlock.defaultPageSize)(implicit bs: Serializer) extends PaginatedArray[Byte](pageSize) {
  import MemoryBlock._
  import BasicSerializer._

  /**
   * Write a value
   * @param v Value to write
   * @return Number of written bytes
   */
  def poke(v: Any): Int = {
    val bav = bs.anyToBytes(v)
    val bac = bs.anyToCode(v, bav.length)

    synchronized[Int] {
      writeElements(bac) + writeElements(bav)
    }
  }

  def poke(l: List[Any]): Int = {
    val items = for (v <- l) yield { insert(v) }
    val bav = bs.intListToBytes(items)
    val bac = bs.anyToCode(l, bav.length)

    synchronized[Int] {
      writeElements(bac) + writeElements(bav)
    }
  }

  /**
   * Insert a new value
   * @param v Value to write
   * @return Offset of new value
   */
  def insert(v: Any): Int = synchronized[Int]({
    val offset: Int = getFreeOffset
    poke(v)
    offset
  })

  /**
   * Insert a new value
   * @param l List of values to write
   * @return Offset of the list
   */
  def insert(l: List[Any]): Int = {
    val items = for (v <- l) yield { insert(v) }
    val bav = bs.intListToBytes(items)
    val bac = bs.anyToCode(l, bav.length)

    synchronized[Int] {
      val offset: Int = getFreeOffset
      writeElements(bac) + writeElements(bav)
      offset
    }
  }

  /**
   * Append a value to an existing list
   * @param offset Offset of original list
   * @param v Value to append (at the end)
   * @return Offset of the new list
   * @note The original list is untouched
   */
  def append(offset: Int, v: Any): Int = {
    val ov = insert(v)

    val items: List[Int] = List(offset, ov)
    val bav = bs.intListToBytes(items)
    val bac = bs.anyToCode(items, bav.length)

    synchronized[Int] {
      val offset: Int = getFreeOffset
      writeElements(bac) + writeElements(bav)
      offset
    }
  }

  /**
   * Append a list to an existing list
   * @param offset Offset of original list
   * @param l Value to append (at the end)
   * @return Offset of the new list
   * @note The original list is untouched
   */
  def append(offset: Int, l: List[Any]): Int = {
    val items: List[Int] = offset :: (for (v <- l) yield { insert(v) })

    val bav = bs.intListToBytes(items)
    val bac = bs.anyToCode(items, bav.length)

    synchronized[Int] {
      val offset: Int = getFreeOffset
      writeElements(bac) + writeElements(bav)
      offset
    }
  }

  /**
   * Read one byte (with default value)
   */
  private def byteAt(offset: Int, default: Byte = 0): Byte = readElement(offset).getOrElse(default)

  private def typeOf(offset: Int): Byte = (byteAt(offset) & 0x0f).toByte
  
  def isList(offset: Int): Boolean = (typeOf(offset) == LIST)

  private def flattenList(al: List[Any]): List[Any] = al match {
    case Nil => Nil
    case h :: t => h match {
      case sl: List[Any] => flattenList(sl) ::: flattenList(t)
      case _ => h :: flattenList(t)
    }
  }
  
  /**
   * Read a value from paginated byte array
   * @param offset Value offset in array
   * @return Tuple with Option holding the read value and the encoded value length
   */
  private def innerPeek(offset: Int): (Option[Any], Int) = {

    val fb: Byte = byteAt(offset)
    val code: Byte = (fb & 0x0f).toByte
    val clen: Byte = (fb >> 4).toByte

    val vlen: Int =
      if (clen > 0) (1 to clen).fold(0)((a: Int, b: Int) => (a << 8) + byteAt(offset + b))
      else
        bs.getTypeLength(code)

    if (vlen == 0) (None, clen + vlen)
    else {
      val bav: Option[Array[Byte]] = readElements(offset + 1 + clen, vlen)

      if (code == LIST) {
        if (bav.isEmpty) (Some(Nil), 1 + clen + vlen)
        else {
          val items = bs.bytesToIntList(bav.get)
          val list = flattenList(items.map(ix => innerPeek(ix)._1.get))
          (Some(list), 1 + clen + vlen)
        }
      } else {
        if (bav.isEmpty) (None, 1 + clen + vlen)
        else (Some(bs.bytesToAny(code, bav.get)), 1 + clen + vlen)
      }
    }
  }

  /**
   * Read a value from paginated byte array
   * @param offset  Offset where the value is expected
   * @param default Default value if an error occurs
   * @return Read value (or default)
   */
  def peek(offset: Int, default: Any): Any = innerPeek(offset)._1.getOrElse(default)
  def peek(offset: Int): Option[Any] = innerPeek(offset)._1

  
  def bdump(offset: Int = 0): Unit = {
     val (v: Option[Any], s: Int) = innerPeek(offset)
     if (s > 0) {
       println("@" + offset + " -> " + v.getOrElse("nothing"))
       bdump(offset+s)
     }
  }
}