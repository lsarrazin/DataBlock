package org.lsa.scala.data


case object UnexpectedCall {

  def apply[T](default: T): T = {
    println("!!! Unexpected call detected")
    default
  }
}

// TODO: Add copy constructor

trait Node {

  def getCode: Int

  def getName: String
  def setName(name: String): Node

  def getValue: Option[Any]
  def setValue(value: Any): Node

  def getParent: Node
  def setParent(parent: Node): Node

  def getChild: Node
  def addChild(child: Node): Node

  def getSibling: Node
  def addSibling(sibling: Node): Node
  
  def findNode(path: String): Node

  def isEmpty: Boolean
  def length: Int

  def matches(other: Node): Boolean
}

abstract class NodeMapper(node: Int)(implicit nf: NodeFactory) extends Node {

  def getCode: Int = node

  def getChild: Node = nf.getChild(node)
  def addChild(child: Node): Node = {
    nf.addChild(node, child.getCode)
    this
  }

  def getSibling: Node = nf.getSibling(node)
  def addSibling(sibling: Node): Node = {
    nf.addSibling(node, sibling.getCode)
    this
  }

  def isEmpty: Boolean = nf.isEmpty(node)
  def length: Int = nf.length(node)
}

case class NamedNode(node: Int)(implicit nf: NodeFactory) extends NodeMapper(node) {

  def getName: String = nf.getName(node)
  def setName(name: String): Node = {
    nf.setName(node, name)
    this
  }

  def getValue: Option[Any] = nf.getValue(node)
  def setValue(value: Any): Node = {
    nf.setValue(node, value)
    this
  }

  override def getParent: Node = nf.getParent(node)
  override def setParent(parent: Node): Node =  {
    nf.setParent(node, parent.getCode)
    this
  }

  override def matches(other: Node): Boolean = other match {
    case NoNode => false
    case RootNode(_) => false
    case NamedNode(oc: Int) => (oc == node)
  }

  override def findNode(path: String): Node = nf.findNode(node, path)

}

case class RootNode(node: Int)(implicit nf: NodeFactory) extends NodeMapper(node) {

  // Root node holds code 0
  override def getCode: Int = 0

  // Root node has no name
  override def getName: String = UnexpectedCall[String]("")
  override def setName(name: String): Node = UnexpectedCall[Node](this)

  // Root node has no value
  override def getValue: Option[Any] = UnexpectedCall[Option[Any]](None)
  override def setValue(value: Any): Node = UnexpectedCall[Node](this)

  // Root node has no parent
  override def getParent: Node = NoNode
  override def setParent(parent: Node): Node = UnexpectedCall[Node](this)

  // Root node has no sibling
  override def getSibling: Node = NoNode
  override def addSibling(sibling: Node): Node = UnexpectedCall[Node](this)

  override def findNode(path: String): Node = nf.findNode(node, path)

  override def matches(other: Node): Boolean = (other.getCode == 0)
}

case object NoNode extends Node {

  // NoNode fakes code -1
  override def getCode: Int = -1
  
  // NoNode has no name
  override def getName: String = UnexpectedCall[String]("")
  override def setName(name: String): Node = UnexpectedCall[Node](this)

  // NoNode has no value
  override def getValue: Option[Any] = UnexpectedCall[Option[Any]](None)
  override def setValue(value: Any): Node = UnexpectedCall[Node](this)

  // NoNode has no parent
  override def getParent: Node = UnexpectedCall[Node](this)
  override def setParent(parent: Node): Node = UnexpectedCall[Node](this)

  override def getChild: Node = UnexpectedCall[Node](this)
  override def addChild(child: Node): Node = UnexpectedCall[Node](this)

  override def getSibling: Node = UnexpectedCall[Node](this)
  override def addSibling(sibling: Node): Node = UnexpectedCall[Node](this)

  override def isEmpty: Boolean = UnexpectedCall[Boolean](true)
  override def length: Int = UnexpectedCall[Int](0)

  override def findNode(path: String): Node = UnexpectedCall[Node](NoNode)

  override def matches(other: Node): Boolean = (other.getCode == -1)
  
}

/*
object Node {

  def newNamedNode(name: Int, code: Int, parent: Int): Node = NamedNode(name, code, parent, None, None, None)
  
  def put(node: Node, name: String, value: Any)(implicit nr: NodeRunner): Node = {
    val cnode: Node = node.getChild(name)
    if (cnode == NoNode) {
      // Child do not exist, allocate a new one
      val nnode = nr.nodeBuilder(node, name, value)
      nnode
    } else {
      // Existing child node, update node
      cnode
    }
  }
  
  private def nodePath(path: String): List[String] = {
    if (path.length == 0) Nil
    else if (((path.length == 1) && (path(0) == '/')) || (path == "//")) List("")
    else path.split('/').toList
  }
  
  private def walkTree(node: Node, path: List[String])(implicit nr: NodeRunner): Node = {
    if (path.head == "") nr.nodeFinder(nr.rootNode, path.tail.filter(_ != ""))
    else nr.nodeFinder(node, path.filter(_ != ""))
  }
  
  def get(node: Node, name: String)(implicit nr: NodeRunner): Option[Any] = {
    val vnode: Node = walkTree(node, nodePath(name))
    nr.valueReader(vnode)
  }
  
  def getParent(current: Node)(implicit nr: NodeRunner): Node = ???  
  * 
  private def nodePath(path: String): List[String] = {
    if (path.length == 0) Nil
    else if (((path.length == 1) && (path(0) == '/')) || (path == "//")) List("")
    else path.split('/').toList
  }
  
  private def walkTree(node: Node, path: List[String])(implicit nr: NodeRunner): Node = {
    if (path.head == "") nr.nodeFinder(nr.rootNode, path.tail.filter(_ != ""))
    else nr.nodeFinder(node, path.filter(_ != ""))
  }
  
  def get(node: Node, name: String)(implicit nr: NodeRunner): Option[Any] = {
    val vnode: Node = walkTree(node, nodePath(name))
    nr.valueReader(vnode)
  }
  
  def getChild(current: Node)(implicit nr: NodeRunner): Node = ???
  
  def getSibling(current: Node)(implicit nr: NodeRunner): Node = ???
  
  def getChild(current: Node, name: String)(implicit nr: NodeRunner): Node = nr.childFinder(current, name)
  
  def getSibling(current: Node, name: String)(implicit nr: NodeRunner): Node = ???
  
  def countChildren(elder: Int)(implicit nr: NodeRunner): Int = 
    nr.nodeLoader(elder).next.fold(0)(1 + countChildren(_))

  def length(child: Option[Int], value: Option[Int])(implicit nr: NodeRunner): Int = {
    val vc = if (value.isDefined) 1 else 0
    val cc = if (child.isDefined) countChildren(child.get) else 0
    
    vc + cc
  }
}

*/