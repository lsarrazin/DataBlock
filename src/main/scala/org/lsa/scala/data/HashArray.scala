package org.lsa.scala.data

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3
import java.nio.CharBuffer

class HashArray(pageSize: Int, nameSize: Int) extends PaginatedArray[Int](pageSize * 3) {

  val data: PaginatedArray[Char] = new PaginatedArray[Char](nameSize) 

  def readName(entry: Int): Int = readElement(entry * 3 + 0).get
  def readHash(entry: Int): Int = readElement(entry * 3 + 1).get
  def readNext(entry: Int): Int = readElement(entry * 3 + 2).get
  def readEntry(entry: Int): (Int, Int, Int) = {
    val e: Array[Int] = readElements(entry * 3, 3).getOrElse(Array[Int](-1, -1, -1))
    (e(0), e(1), e(2))
  }

  def writeName(entry: Int, name: Int): Unit = updateElement(entry * 3 + 0, name)
  def writeHash(entry: Int, hash: Int): Unit = updateElement(entry * 3 + 1, hash)
  def writeNext(entry: Int, next: Int): Unit = updateElement(entry * 3 + 2, next)
  def writeEntry(entry: Int, name: Int, hash: Int, next: Int): Int =
    updateElements(entry * 3, Array[Int](name, hash, next))
  def insertEntry(name: Int, hash: Int, next: Int): Int = insertElements(Array(name, hash, next)) / 3

  override def length: Int = super.length / 3
  
  /**
   * Initialize HashArray to allocate the first pageSize buckets
   */
  override def initNewPage(order: Int, newPage: PaginatedArray[Int]): PaginatedArray[Int] = {
    super.initNewPage(order, newPage)

    if (order == 0) {
      // Initialize first page
      newPage.writeElements(Array.fill[Int](pageSize * 3)(-1))
    }
    newPage
  }

  /**
   * Hash function
   * @param s String to hash
   * @return Hashcode of s
   */
  def hashString(s: String) = MurmurHash3.stringHash(s)

  /**
   * Compute hash table distribution
   * @return Array[Int]: a table with the number of values each bucket holds
   */
  def distribution: Array[Int] = {

    def count(i: Int): Int = {
      if (i < 0) 0
      else {
        val (name: Int, hash: Int, next: Int) = readEntry(i)
        if (name == -1) 0 else 1 + count(next)
      }

      0
    }

    (0 until pageSize).map { count(_) }.toArray
  }

  /**
   * Compute hash table load factor
   * @return load factor
   */
  def getLoadFactor: Float =
    (0 until pageSize).fold(0) { (a: Int, b: Int) => a + (if (readName(b) == -1) 0 else 1) } / pageSize

  /**
   * Lookup a string and return hash index
   * @param s String to lookup
   * @return Id of the stored string, -1 if not found
   */
  def fetch(s: String): Int = {

      @tailrec
    def bucketLookup(ie: Int, h: Int): Int = {
      val (name: Int, hash: Int, next: Int) = readEntry(ie)
      if (name == -1) -1
      else if (hash == h) ie
      else if (next == -1) -1
      else bucketLookup(next, h)
    }

    val sh: Int = hashString(s)
    val b: Int = math.abs(sh) % pageSize
    bucketLookup(b, sh)
  }

  /**
   * Store new string to data array
   * @param s String to store
   * @return Offset of new string, or -1 in case of failure
   */
  private def putString(s: String): Int = {
    val l: Int = s.length
    val sa: Array[Char] = new Array[Char](l + 1)
    s.copyToArray(sa)
    sa(l) = 0

    data.insertElements(sa)
  }

  /**
   * Get back string from data array
   * @param hp Offset of stored string
   * @return read string from array, empty in case of failure
   */
  private def getString(hp: Int): String = {
    var np: Int = hp
    val builder = StringBuilder.newBuilder
    while (np >= hp) {
      val c = data.readElement(np).get
      if (c > 0) {
        builder += c
        np += 1
      } else np = -1
    }

    builder.toString
  }

  /**
   * Append a new string to array
   * @param s element string
   * @return index of the element (added or existing)
   */
  def put(s: String): Int = {

    @tailrec
    def appendToBucket(b: Int, s: String, h: Int): Int = {
      val (name: Int, hash: Int, next: Int) = readEntry(b)
      if (hash == h) h // Duplicate!
      else if (next == -1) {
        // Append new entry
        val ne: Int = insertEntry(putString(s), h, -1)
        writeNext(b, ne)
        ne
      } 
      else appendToBucket(next, s, h)
    }

    val h: Int = hashString(s)
    val b: Int = scala.math.abs(h) % pageSize
    val (name: Int, hash: Int, next: Int) = readEntry(b)
    
    // Search string in bucket
    if (name == -1) {
      // Update bucket with first element
      writeEntry(b, (putString(s)), h, -1)
      b
    } 
    else if (hash == h) h // Duplicate
    else appendToBucket(b, s, h)
  }

  /**
   * Get recorded string from its hash value
   * @param hash Hash of string
   * @return Recorded string, or None
   */
  def get(hash: Int): Option[String] = {

    def getFromBucket(b: Int, hash: Int): Option[String] = {
      if (readHash(b) == hash) Some(getString(readName(b)))
      else {
        val n = readNext(b)
        if (n == -1) None else getFromBucket(n, hash)
      }
    }

    val b: Int = scala.math.abs(hash) % pageSize
    val n: Int = readName(b)

    // Search string in bucket
    if (n == -1) None
    else if (readHash(b) == hash) Some(getString(n))
    else getFromBucket(b, hash)
  }

  /**
   * Get recorded string from its hash value
   * @param hash Hash of string
   * @return Recorded string, or None
   */
  def lookup(index: Int): Option[String] = {
    
    val nidx: Int = readName(index)
    if (nidx == -1) None
    else Some(getString(nidx))
  }

  /**
   *
   */
  def at(h: Int): String = {
    val n: Int = readName(h)
    if (n == -1) "[" + h + ": <Empty>]"
    else {
      val name: String = getString(n)
      val next: Int = readNext(h)
      if ((next >= 0) && (next < h)) name + " !!! Infinite loop"
      else if (next != -1) "[" + h + ": " + name + ", " + next + "], " + at(next)
      else "[" + h + ": " + name + ", " + next + "]"
    }
  }

  def dump: Unit = {
    for (i <- 0 until pageSize) {
      println(" . " + at(i))
    }
  }
}
