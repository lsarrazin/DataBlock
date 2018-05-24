package org.lsa.scala.data

import scala.annotation.tailrec

// TODO: Clean comments
// TODO: Complete Int approach
// TODO: Remove Node.scala

/*
trait NodeRunner {
  def nodeLoader(ncode: Int): Node
  def nodeBuilder(parent: Node, name: String, value: Any): Node
  def nodeFinder(start: Node, path: List[String]): Node
  
  def childFinder(parent: Node, name: String): Node
  
  def valueReader(node: Node): Option[Any]
  
  def rootNode: Node
}
*
*/
// TODO: Add value made of another (Data)Node
// TODO: Add value made of List

class NodeFactory(treeSize: Int = 128, dataSize: Int = 2048) /*extends NodeRunner*/ {
  
  implicit val nf: NodeFactory = this
  
  private val tree: PaginatedArray[Int] = new PaginatedArray[Int](treeSize * 5)
  private val data: MemoryBlock = MemoryBlock.newBlock(dataSize)
  private val hash: HashArray = new HashArray(treeSize, dataSize)
  
  private val emptyNode: Array[Int] = Array[Int](-1, -1, -1, -1, -1)
  private def leafNode(name: String, value: Any): Array[Int] = Array[Int](hash.put(name), data.insert(value), -1, -1, -1)
  
  private def readName(node: Int): Int = tree.readElement(node + 0).getOrElse(-1)
  private def readValue(node: Int): Int = tree.readElement(node + 1).getOrElse(-1)
  private def readParent(node: Int): Int = tree.readElement(node + 2).getOrElse(-1)
  private def readChild(node: Int): Int = tree.readElement(node + 3).getOrElse(-1)
  private def readSibling(node: Int): Int = tree.readElement(node + 4).getOrElse(-1)
  
  private def writeName(node: Int, ncode: Int): Unit = tree.updateElement(node + 0, ncode)
  private def writeValue(node: Int, vcode: Int): Unit = tree.updateElement(node + 1, vcode)
  private def writeParent(node: Int, pcode: Int): Unit = tree.updateElement(node + 2, pcode)
  private def writeChild(node: Int, ccode: Int): Unit = tree.updateElement(node + 3, ccode)
  private def writeSibling(node: Int, scode: Int): Unit = tree.updateElement(node + 4, scode)
  
  private def writeNode(name: Int, value: Int, parent: Int, child: Int, sibling: Int): Int = 
    tree.insertElements(List(name, value, parent, child, sibling).toArray)
  
  def isEmpty = (tree.length == 0)
  def length = (tree.length / 5)
  
  // Insert root node
  tree.insertElements(emptyNode)
  val root: RootNode = RootNode(0)
  
  def newNode: NamedNode = NamedNode(tree.insertElements(emptyNode))
  def newNode(name: String, value: Any): NamedNode = NamedNode(tree.insertElements(leafNode(name, value)))
  
  def setName(node: Int, name: String): Unit = writeName(node, hash.put(name))
  def getName(node: Int): String = hash.get(readName(node)).getOrElse("")  
  
  def setValue(node: Int, value: Any): Unit = writeValue(node, data.insert(value))
  def getValue(node: Int): Option[Any] = data.peek(readValue(node))
  
  def getParent(node: Int): Node = {
    val parent: Int = readParent(node)
    if (parent == 0) RootNode(0)
    else NamedNode(parent)
  }
  
  def setParent(node: Int, parent: Int): Unit = {
    // Detach from any previous parent
    val op: Int = readParent(node)
    if (op != -1) detachChild(op, node)
    
    // Attach to children
    if (attachChild(op, node)) writeParent(node, parent)
  }
  
  @tailrec
  final def detachSibling(node: Int, sibling: Int): Boolean = {
    val ns: Int = readSibling(node)
    if (ns == -1) false
    else if (ns == sibling) {
      writeSibling(node, readSibling(sibling))
      writeParent(sibling, -1)
      true
    }
    else detachSibling(ns, sibling)
  }
  
  def detachChild(node: Int, child: Int): Boolean = {
    val fc: Int = readChild(node)
    if (fc == -1) false
    else if (fc == child) {
      writeChild(node, readSibling(child))
      writeParent(child, -1)
      true
    } else detachSibling(fc, child)
  }
  
  @tailrec
  final def attachSibling(node: Int, sibling: Int): Boolean = {
    val ns: Int = readSibling(node)
    if (ns == -1) {
      writeSibling(node, sibling)
      writeParent(sibling, readParent(node))
      true
    } else attachSibling(ns, sibling)
  }
  
  def attachChild(node: Int, child: Int): Boolean = {
    val fc: Int = readChild(node)
    if (fc == -1) {
      writeChild(node, child)
      writeParent(child, node)
      true
    } else attachSibling(fc, child)
  }
  
  def getChild(node: Int): Node = {
    val fc: Int = readChild(node)
    if (fc == -1) NoNode
    else NamedNode(fc)
  }
  
  def addChild(node: Int, child: Int): Unit = attachChild(node, child)
 
  def getSibling(node: Int): Node = {
    val ns: Int = readSibling(node)
    if (ns == -1) NoNode
    else NamedNode(ns)
  }
  
  def addSibling(node: Int, sibling: Int): Unit = attachSibling(node, sibling)

  def findChild(node: Int, name: String): Int = {
    
    @tailrec
    def siblingFinder(node: Int, name: Int): Int = {
      if (readName(node) == name) node
      else {
        val sibling: Int = readSibling(node)
        if (sibling == -1) -1
        else siblingFinder(sibling, name)
      }
    }
    
    val nidx: Int = hash.fetch(name)
    if (nidx == -1) -1
    else {
      val child: Int = readChild(node)
      if (child == -1) -1
      else siblingFinder(child, nidx)
    }
  }
  
  private def nodePath(path: String): List[String] = {
    if (path.length == 0) Nil
    else if (((path.length == 1) && (path(0) == '/')) || (path == "//")) List("")
    else path.split('/').toList
  }
  
  @tailrec
  final def findNode(node: Int, path: List[String]): Int = {
    if ((path == Nil) || (node == -1)) node
    else {
      val nname: String = path.head
      val child: Int = findChild(node, nname)
      if (child == -1) -1
      else findNode(child, path.tail)
    }
  }
  
  private def walkTree(node: Int, path: List[String]): Int = {
    if (path.head == "") findNode(0, path.tail.filter(_ != ""))
    else findNode(node, path.filter(_ != ""))
  }
  
  def findNode(node: Int, name: String): Node = {
    val code: Int = walkTree(node, nodePath(name))
    if (code == -1) NoNode
    else if (code == 0) RootNode(code)
    else NamedNode(code)
  }
  
  def isEmpty(node: Int): Boolean = (readValue(node) != -1) || (readChild(node) != -1)
 
  final def childrenLength(node: Int): Int = 
    if (node == -1) 0 else (length(node) + childrenLength(readSibling(node)))
    
  def length(node: Int): Int = {
    if (node == -1) 0
    else (if (readValue(node) == -1) 0 else 1) + childrenLength(readChild(node)) 
  }

  def dump(node: Int, pad: Int = 0): Unit = {
    val pstr: String = " " * pad
    
    val nidx: Int = readName(node)
    val vidx: Int = readValue(node)
    (nidx, vidx) match {
      case (-1, -1) => println(pstr + "<<Unnamed empty>>")
      case (-1, _) => println(pstr + "<<Unnamed>> = " + data.peek(vidx, "<<Erroneous>>"))
      case (_, -1) => println(pstr + hash.lookup(nidx).getOrElse("<<Unnamed>>") + " = ")
      case _ => println(pstr + hash.lookup(nidx).getOrElse("<<Unnamed>>") + " = " + data.peek(vidx, "<<Erroneous>>"))
    }
    
    val child: Int = readChild(node)
    if (child != -1) dump(child, pad + 1)
    
    val sibling: Int = readSibling(node)
    if (sibling != -1) dump(sibling, pad)
  }

}

