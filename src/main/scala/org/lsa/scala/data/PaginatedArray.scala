package org.lsa.scala.data

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
 * Implementation of paginated array
 * @tparam T Type of array elements
 * @param pageSize Size of each page
 * @param order Page order counter (0 for first page)
 */

class PaginatedArray[T: ClassTag](pageSize: Int, order: Int = 0) {

  val page: Array[T] = new Array[T](pageSize)
  var pptr: Int = 0;
  var nextPage: Option[PaginatedArray[T]] = None
  var freePage: PaginatedArray[T] = this

  initNewPage(order, this)

  type Location = (PaginatedArray[T], Int)

  /**
   * Get current page order
   * @return Page order
   */
  def getOrder: Int = order

  /**
   * Page initializer
   *
   * Feel free to extend this function to initialize any newly created page
   * @return Current page
   */
  def initNewPage(order: Int, newPage: PaginatedArray[T]): PaginatedArray[T] = {
    newPage.nextPage = None
    newPage.freePage = newPage
    newPage.pptr = 0
    newPage
  }

  /**
   * Get free element offset
   * @return element offset
   */
  def getFreeOffset: Int = ((freePage.getOrder * pageSize) + freePage.pptr)

  /**
   * Seek an array page from its page order
   * @param porder Array page order to find
   * @return Found array page as Option
   */
  @tailrec
  final def seekPage(porder: Int): Option[PaginatedArray[T]] = {
    if (porder == order) Some(this)
    else if (porder < order || nextPage.isEmpty) None
    else nextPage.get.seekPage(porder)
  }

  /**
   * Seek an element from its offset
   * @param ptr Element offset
   * @return Found element location as Option[(PaginatedArray, Int)]
   */
  def seek(offset: Int): Option[Location] = {
    if (offset < 0) None else {
      val o: Int = offset / pageSize
      val p: Int = offset % pageSize

      val spage: Option[PaginatedArray[T]] = seekPage(o)
      if (spage.isEmpty) None
      else Some((spage.get, p))
    }
  }

  /**
   * Get array length
   * @return size of written data (T) on all pages
   */
  def length: Int =
    nextPage.foldLeft(pptr)((a: Int, b: PaginatedArray[T]) => a + b.length)

  /**
   * Get array size
   * @return size of allocated data from all pages
   */
  def size: Int =
    nextPage.foldLeft(pageSize)((a: Int, b: PaginatedArray[T]) => a + b.size)

  /**
   * Reset paginated array
   * @param free release additional pages if set to true
   */
  def reset(free: Boolean): PaginatedArray[T] = {
    nextPage.map(_.reset(free))
    if (free) nextPage = None
    pptr = 0
    freePage = this
    this
  }

  /**
   * Add a page to current array for more storage
   *  @return newly allocated page
   */
  def newPage: PaginatedArray[T] = {
    freePage = nextPage.getOrElse {
      val newPage = new PaginatedArray[T](pageSize, order + 1)
      nextPage = Some(newPage)
      initNewPage(order + 1, newPage)
    }

    freePage
  }

  /**
   * Update an element with a new one
   * @param ptr Offset to element to update
   * @param elt Updated element
   * @return Number of updated element (1 or 0 if out of bound)
   */
  def updateElement(offset: Int, elt: T): Int = {
    val (pa: PaginatedArray[T], pp: Int) = seek(offset).get
    pa.page.update(pp, elt)
    1
  }

  /**
   * Write a single element to array
   *
   * @param elt element to write
   * @return number of written elements
   * @note: New pages are allocated if required
   */
  def writeElement(elt: T): Int = {
    val tpage = if (freePage.pptr == pageSize) {
      freePage = freePage.newPage
      freePage
    } else freePage

    tpage.page.update(tpage.pptr, elt)
    tpage.pptr += 1
    1
  }

  /**
   * Append a single element to array
   * @param elt Element to write
   * @return offset of written element, or -1 if failed
   */
  def insertElement(elt: T): Int = synchronized[Int] {
    val offset: Int = getFreeOffset
    if (writeElement(elt) == 1) offset
    else -1
  }

  /**
   * Append a single element to array using element assigned index
   * @param f Element builder from index (Partial function)
   * @return offset of written element, or -1 if failed
   */
  def insertWithIndex(f: Int => T)(default: T): T = {
    synchronized[T] {
      val offset: Int = getFreeOffset
      val elt: T = f(offset)
      if (writeElement(f(offset)) == 1) elt
      else default
    }
  }

  /**
   * Update a chunk of elements from the provided offset
   *
   * @param elts elements to write
   * @param from first element to consider from elts
   * @param to   last element to consider (-1 for all)
   * @return number of written elements
   * @note: New pages are allocated if required
   */
  def updateElements(offset: Int, elts: Array[T], from: Int = 0, to: Int = -1): Int = {
 
    def innerUpdateElements(loc: Location, elts: Array[T], from: Int = 0, to: Int = -1): Int = {
      val es: Int = if (to == -1) (elts.length - from) else (to - from)
      val cs: Int = scala.math.min(pageSize - loc._2, es);
      Array.copy(elts, from, loc._1.page, loc._2, cs)
      
      if (es > cs) {
        cs + innerUpdateElements((loc._1.nextPage.get, 0), elts, from + cs, to)
      } else {
        cs
      }
    }

    innerUpdateElements(seek(offset).get, elts, from, to)    
  }

  /**
   * Write a chunk of elements to the end of the paginated array
   *
   * @param elts elements to write
   * @param from first element to consider from elts
   * @param to   last element to consider (-1 for all)
   * @return number of written elements
   * @note: New pages are allocated if required
   */
  def writeElements(elts: Array[T], from: Int = 0, to: Int = -1): Int = {
    val es: Int = if (to == -1) (elts.length - from) else (to - from)
    val cs: Int = scala.math.min(pageSize - freePage.pptr, es);

    Array.copy(elts, from, freePage.page, freePage.pptr, cs)
    freePage.pptr += cs

    if (es > cs) {
      // Remaining elements to write
      freePage = freePage.newPage
      cs + freePage.writeElements(elts, from + cs, to)
    } else {
      cs
    }
  }

  /**
   * Append a chunk of elements to the end of the paginated array
   *
   * @param elts elements to write
   * @param from first element to consider from elts
   * @param to   last element to consider (-1 for all)
   * @return number of written elements
   * @note: New pages are allocated if required
   */
  def insertElements(elts: Array[T], from: Int = 0, to: Int = -1): Int = synchronized[Int] {
    val offset: Int = getFreeOffset
    if (writeElements(elts, from, to) > 0) offset
    else -1
  }

  /**
   * Read one element from location
   *  @param loc Element location
   *  @return Option containing the read element
   */
  private def innerReadElement(loc: Option[(PaginatedArray[T], Int)]): Option[T] = {
    if (loc.isDefined) {
      val (pa: PaginatedArray[T], pp: Int) = loc.get
      Some(pa.page(pp))
    } else {
      None
    }
  }

  /**
   * Read one element from offset
   *  @param offset Offset to the element to read
   *  @return Option containing the read element
   */
  def readElement(offset: Int): Option[T] = innerReadElement(seek(offset))

  /**
   * Copy many elements to array
   * @param par  Starting page array
   * @param from Starting page offset
   * @param tgt  Target array
   * @param to   Target offset
   * @param len  Number of elements to copy
   * @return Number of copied elements
   */
  private def innerReadElements(par: PaginatedArray[T], from: Int, tgt: Array[T], to: Int, len: Int): Int = {
    if (from + len < pageSize) {
      Array.copy(par.page, from, tgt, to, len)
      len
    } else {
      val cs: Int = pageSize - from
      Array.copy(par.page, from, tgt, to, cs)
      if (par.nextPage.isEmpty) cs else cs + innerReadElements(par.nextPage.get, 0, tgt, to + cs, len - cs)
    }
  }

  /**
   * Read many elements
   * @param loc  Starting location (page array and offset)
   * @param len  Number of elements to read
   * @return Option containing an array with read elements (or None)
   */
  private def innerReadElements(loc: Option[(PaginatedArray[T], Int)], len: Int): Option[Array[T]] = {
    if (loc.isDefined) {
      val (par: PaginatedArray[T], from: Int) = loc.get
      val tgt: Array[T] = new Array[T](len)
      val szr: Int = innerReadElements(par, from, tgt, 0, len)
      if (szr == len) Some(tgt) else None
    } else {
      None
    }
  }

  /**
   * Read many elements
   * @param offset Offset to the element to read
   * @param len  Number of elements to read
   * @return Option containing an array with read elements (or None)
   */
  def readElements(offset: Int, len: Int): Option[Array[T]] = innerReadElements(seek(offset), len)

  /**
   * Clone paginated array into a new one
   *  @return cloned paginated array
   *  @note Original paginated array is untouched
   */
  override def clone: PaginatedArray[T] = {
    val newPA = new PaginatedArray[T](pageSize, order)
    page.copyToArray(newPA.page)
    newPA.pptr = pptr

    if (nextPage.isDefined) {
      newPA.nextPage = Some(nextPage.get.clone)
      newPA.freePage = newPA.nextPage.get.freePage
    } else {
      newPA.freePage = this
    }
    newPA
  }

  /**
   * Get page content as string (for debug purpose only)
   * @param data Page array
   * @param dptr Page content size
   * @param elemToString Function that turns any value of T into a string
   * @return Whole paginated array as string
   */
  private def pageAsString(data: Array[T], dptr: Int, elemToString: T => String): String =
    data.slice(0, dptr).map(elemToString).mkString(".")

  /**
   * Get array content as string (for debug purpose only)
   * @param elemToString Function that turns any value of T into a string
   * @return Whole paginated array as string
   */
  def asString(elemToString: T => String = (e: T) => e.toString): String = {
    nextPage.foldLeft(pageAsString(page, pptr, elemToString))((a: String, b: PaginatedArray[T]) => a + "|" + b.asString(elemToString))
  }

  /**
   * Dump array content to console (for debug purpose only)
   * @param recurse If set, dump additional pages
   * @param elemToString Function that turns any value of T into a string
   */
  def dump(recurse: Boolean = false, elemToString: T => String = (e: T) => e.toString): Unit = {
    println("PA@" + this + ", pptr=" + pptr + ", nextPage=" + nextPage + ", freePage=" + freePage)
    println(" - " + (if (recurse) asString(elemToString) else pageAsString(page, pptr, elemToString)))
    if (recurse) nextPage.foreach(_.dump(recurse))
  }

}