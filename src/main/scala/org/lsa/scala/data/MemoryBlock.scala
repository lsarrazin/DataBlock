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
}

object BasicSerializer extends Serializer {

  def encodingLength(s: Int): Byte = {
    if (s < (1 << 8)) 1
    else if (s < (1 << 16)) 2
    else if (s < (1 << 24)) 3
    else 4
  }

  def getTypeLength(code: Byte): Int = code match {
    case 1 => 1
    case 2 => 1
    case 3 => 8
    case 4 => 2
    case 5 => 4
    case 6 => 8
    case _ => 0
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
    case l: List[Any] => {
      val bb = ByteBuffer.allocate(0)
      l.foreach { i: Any => 
        bb.put(anyToBytes(i), 0, -1)
      }
      bb.array
    }
  }

  def bytesToAny(t: Byte, v: Array[Byte]): Any = t match {
    case 1 => if (v(0) == 1) true else false
    case 2 => v(0)
    case 3 => ByteBuffer.wrap(v).asDoubleBuffer.get
    case 4 => ByteBuffer.wrap(v).asShortBuffer.get
    case 5 => ByteBuffer.wrap(v).asIntBuffer.get
    case 6 => ByteBuffer.wrap(v).asLongBuffer.get
    case 7 => new String(v, StandardCharsets.UTF_16)
    case _ => None
  }

  def anyToCode(v: Any): (Byte, Byte) = v match {
    case _: Boolean => (1, 0)
    case _: Byte    => (2, 0)
    case _: Double  => (3, 0)
    case _: Short   => (4, 0)
    case _: Int     => (5, 0)
    case _: Long    => (6, 0)
    case s: String  => (7, encodingLength(s.length))
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

  /**
   * Write a value
   * @param v Value to write
   * @return Number of written bytes
   */
  def poke(v: Any): Int = {
    val bav = bs.anyToBytes(v)
    val bac = bs.anyToCode(v, bav.length)

    writeElements(bac) + writeElements(bav)
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
   * Read one byte (with default value)
   */
  private def byteAt(offset: Int, default: Byte = 0): Byte = readElement(offset).getOrElse(default)

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
      if (bav.isEmpty) (None, 1 + clen + vlen)
      else (Some(bs.bytesToAny(code, bav.get)), 1 + clen + vlen)
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

}