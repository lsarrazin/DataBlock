package org.lsa.scala.data

object DataNode {

  def newNode: DataNode = {
    val factory: NodeFactory = new NodeFactory(256, 4096)
    new RootDataNode(factory.root)(factory)
  }
}

/**
 * Internal DataNode data structure
 *
 * A DataNode is a tree structure allowing <key, value> tuples where any value is either :<ul>
 * <li>a final value</li>
 * <li>a list of values</li>
 * <li>another DataNode</li></ul>
 */
trait DataNode extends Serializable {

  def innerNode: Node
  def newNode: DataNode

  def put(name: String, value: Any): DataNode
  def get(name: String): Option[Any]

  def isEmpty: Boolean
  def length: Int

  def dump: Unit
}

class RootDataNode(root: RootNode)(implicit factory: NodeFactory) extends DataNode {

  def innerNode: Node = root
  def newNode: DataNode = new DataNodeBinder(factory.newNode)

  def put(name: String, value: Any): DataNode = value match {
    case dn: DataNode => {
      dn.innerNode.setName(name)
      root.addChild(dn.innerNode)
      this
    }
    case _ => {
      root.addChild(factory.newNode(name, value))
      this
    }
  }

  def get(name: String): Option[Any] = {
    val node: Node = factory.root.findNode(name)
    node.getValue
  }

  def isEmpty: Boolean = factory.isEmpty
  def length: Int = factory.length

  def dump: Unit = factory.dump(root.getCode)
}

class DataNodeBinder(bound: NamedNode)(implicit factory: NodeFactory) extends DataNode {

  def innerNode: Node = bound
  def newNode: DataNode = new DataNodeBinder(factory.newNode)

  def put(name: String, value: Any): DataNode = value match {
    case dn: DataNode => {
      dn.innerNode.setName(name)
      bound.addChild(dn.innerNode)
      this
    }
    case _ => {
      bound.addChild(factory.newNode(name, value))
      this
    }
  }

  def get(name: String): Option[Any] = {
    val node: Node = bound.findNode(name)
    node.getValue
  }

  def isEmpty: Boolean = bound.isEmpty
  def length: Int = bound.length

  def dump: Unit = factory.dump(bound.getCode)
}

