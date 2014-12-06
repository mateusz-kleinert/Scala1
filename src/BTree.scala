/**
 * Created by Mateusz Kleinert on 28.11.14.
 */

import scala.collection.mutable.Queue

case class BTree[T](level: Int = 2, leafLevel: Int = 3, nodesList: List[Any] = List()) extends BTreeNode {
  val nodes = nodesList

  if (level < 2) {
    throw new IllegalArgumentException("Argument 'level' must be greater than 1.")
  }

  if (leafLevel < 2) {
    throw new IllegalArgumentException("Argument 'leafLevel' must be greater than 1.")
  }

  def getIndex(key: Int): Int = {
    var index = 0

    for (e <- nodes) {
      e match {
        case i: Int => {
          if (i <= key) {
            index = nodes.indexOf(e) + 1
          }
        }
        case _ =>
      }
    }

    return index
  }

  def DFSTraversal (debug: Boolean = false): Unit = {
    for (n <- nodes) {
      n match {
        case e: BTree[T] => {
          e.DFSTraversal(debug)
        }
        case e: BTreeLeaf[T] => {
          e.printLeaf()
        }
        case _ => {}
      }
    }

    if (debug)
      printTree()
  }

  def BFSTraversal (debug: Boolean = false): Unit = {
    var queue = new Queue[List[Any]]
    queue.enqueue(nodes)

    while (!queue.isEmpty) {
      val nodeElements = queue.dequeue

      for (element <- nodeElements) {
        element match {
          case e: Int => {
            if (debug) {
              print(e + " ")
            }
          }
          case e: BTree[T] => {
            if (debug) {
              print("BTree ")
            }
            queue.enqueue(e.nodes)
          }
          case e: BTreeLeaf[T] => {
            if (debug) {
              print("BTreeLeaf ")
            }
            queue.enqueue(e.elements)
          }
          case e: BTreeElement[T] => print(e.getKey + " ")
          case Nil => {
            if (debug) {
              print("Nil ")
            }
          }
          case _ =>
        }
      }

      if (debug) {
        println()
      }
    }
  }

  def printTree () = {
    println()
    println("<--- BTree --->")

    for (n <- nodes) {
      n match {
        case e: Int => print(e + " ")
        case e: BTree[T] => print("BTree ")
        case e: BTreeLeaf[T] => print("BTreeLeaf ")
        case Nil => print("Nil ")
        case _ => print("? ")
      }
    }
  }

  def addElement(value: T, key: Int): BTree[T] = {
    val element: BTreeElement[T] = new BTreeElement[T](value, key)

    if (nodes.length.equals(0)) {
      val leaf = new BTreeLeaf[T](leafLevel)
      val newLeaf = leaf.addElement(element).head
      return new BTree[T](level, leafLevel, List(Nil, element.getKey, newLeaf))
    } else {
      val index: Int = getIndex(element.getKey)

      if (nodes.slice(index, index + 1).head != Nil) {
        val subNode = nodes.slice(index, index + 1).head

        subNode match {
          case e: BTree[T] => {
            val newNode = e.addElement(element.getValue, element.getKey)

            val newNodesList = nodes.slice(0, index) ::: newNode :: nodes.slice(index + 1, nodes.length)

            if (newNodesList.length > (level * 2 - 1)) {
              val divideIndex = (newNodesList.length / 2) + ((newNodesList.length / 2) % 2) + 1

              val leftNode = newNodesList.slice(0, divideIndex)
              val rightNode = newNodesList.slice(divideIndex + 1, newNodesList.length)
              val newDivider = newNodesList.slice(divideIndex, divideIndex + 1).head

              if (rightNode.length.equals(1)) {
                val newLeftNode = new BTree[T](level, leafLevel, leftNode)
                return new BTree[T](level, leafLevel, List(newLeftNode, newDivider, rightNode.head))
              } else {
                val newLeftNode = new BTree[T](level, leafLevel, leftNode)
                val newRightNode = new BTree[T](level, leafLevel, rightNode)
                return new BTree[T](level, leafLevel, List(newLeftNode, newDivider, newRightNode))
              }
            } else {
              return new BTree[T](level, leafLevel, newNodesList)
            }
          }
          case e: BTreeLeaf[T] => {
            val newNode = e.addElement(element)

            if (newNode.length.equals(1)) {
              val newNodesList = nodes.slice(0, index) ::: newNode ::: nodes.slice(index + 1, nodes.length)
              return new BTree[T](level, leafLevel, newNodesList)
            } else {
              val lowerList = newNode.head
              val upperList = newNode.tail.head

              val newDivider = upperList.getFirstElementKey
              val newTree = nodes.slice(0, index) ::: lowerList :: newDivider :: upperList :: nodes.slice(index + 1, nodes.length)
              if (newTree.length > (level * 2 - 1)) {

                val divideIndex = (newTree.length / 2) + (((newTree.length / 2) + 1) % 2)

                val leftNode = newTree.slice(0, divideIndex)
                val rightNode = newTree.slice(divideIndex + 1, newTree.length)
                val newDividerKey = newTree.slice(divideIndex, divideIndex + 1).head

                if (rightNode.length.equals(1)) {
                  val newLeftNode = new BTree[T](level, leafLevel, leftNode)
                  return new BTree[T](level, leafLevel, List(newLeftNode, newDividerKey, rightNode.head))
                } else {
                  val newLeftNode = new BTree[T](level, leafLevel, leftNode)
                  val newRightNode = new BTree[T](level, leafLevel, rightNode)
                  return new BTree[T](level, leafLevel, List(newLeftNode, newDividerKey, newRightNode))
                }
              } else {
                return new BTree[T](level, leafLevel, newTree)
              }
            }
          }
          case _ =>
        }

      } else {
        val leaf = new BTreeLeaf[T](leafLevel)
        val newLeaf = leaf.addElement(element)
        val newNodesList = nodes.slice(0, index) ::: newLeaf ::: nodes.slice(index + 1, nodes.length)
        return new BTree[T](level, leafLevel, newNodesList)
      }
    }

    throw new Exception()
  }

  def getValue(key: Int): T = {
    val index = getIndex(key)
    val node = nodes.slice(index, index + 1).head

    node match {
      case n: BTree[T] => n.getValue(key)
      case n: BTreeLeaf[T] => n.getValue(key)
      case Nil => throw new KeyNotFoundException("Key not found.")
      case _ => throw new KeyNotFoundException("Key not found.")
    }
  }

  def isKey(key: Int): Boolean = {
    val index = getIndex(key)
    val node = nodes.slice(index, index + 1).head

    node match {
      case n: BTree[T] => return n.isKey(key)
      case n: BTreeLeaf[T] => return n.isKey(key)
      case Nil => false
      case _ => false
    }
  }

  def getFirstKey: Int = {
    for (node <- nodes) {
      node match {
        case n: Int => return n
        case n: BTree[T] => return n.getFirstKey
        case n: BTreeLeaf[T] => return n.getFirstElementKey
        case _ =>
      }
    }

    throw new Exception("Empty tree exception.")
  }

  def updateDivider(index: Int, newNode: BTree[T], currentNode: BTree[T]): BTree[T] = {
    if (!index.equals(0)) {
      try {
        val firstKey = newNode.getFirstKey
        val currentSeparator = currentNode.nodes.slice(index - 1, index).head

        currentSeparator match {
          case s: Int => {
            if (s < firstKey) {
              return new BTree[T](level, leafLevel, currentNode.nodes.slice(0, index - 1) ::: firstKey :: newNode :: currentNode.nodes.slice(index + 1, currentNode.nodes.length))
            } else {
              return new BTree[T](level, leafLevel, currentNode.nodes.slice(0, index) ::: newNode :: currentNode.nodes.slice(index + 1, currentNode.nodes.length))
            }
          }
          case _ => throw new Exception("Unknown element exception.")
        }
      } catch {
        case e: Exception => {
          return new BTree[T](level, leafLevel, currentNode.nodes.slice(0, index - 1) ::: Nil :: newNode :: currentNode.nodes.slice(index + 1, currentNode.nodes.length))
        }
      }
    } else {
      return new BTree[T](level, leafLevel, newNode :: currentNode.nodes.slice(1, currentNode.nodes.length))
    }
  }

  def minElementsNumber (level: Int): Int = {
    if (level % 2 == 1) {
      return level
    } else {
      return level - 1
    }
  }

  def removeKey(key: Int): BTree[T] = {
    if (isKey(key)) {
      val index = getIndex(key)
      val node = nodes.slice(index, index + 1).head

      node match {
        case n: BTree[T] => {
          val newNode = n.removeKey(key)
          val newNodeLength = newNode.nodes.filter(_ != Nil).length
          val updatedNodesList = updateDivider(index, newNode, this).nodes

          if (newNodeLength < minElementsNumber(level) || newNodeLength.equals(1)) {
            val rightNode = nodes.slice(index + 2, index + 3)
            val leftNode = nodes.slice(index - 2, index - 1)

            if (!rightNode.isEmpty && rightNode.head != Nil) {
              val node = rightNode.head
              node match {
                case t: BTree[T] => {
                  if (t.nodes.length > minElementsNumber(level) + 1) {
                    val newRightNode = new BTree[T](level, leafLevel, t.nodes.slice(2, t.nodes.length))
                    val newLeftNode = new BTree[T](level, leafLevel, newNode.nodes ::: updatedNodesList.slice(index + 1, index + 2).filter(_ != Nil) ::: t.nodes.slice(0, 1))
                    return new BTree[T](level, leafLevel, updatedNodesList.slice(0, index) ::: newLeftNode :: t.nodes.slice(1, 2) ::: newRightNode :: updatedNodesList.slice(index + 3, updatedNodesList.length))
                  }
                }
                case t: BTreeLeaf[T] =>
                case _ =>
              }
            }

            if (!leftNode.isEmpty && leftNode.head != Nil) {
              val node = leftNode.head
              node match {
                case t: BTree[T] => {
                  if (t.nodes.length > minElementsNumber(level) + 1) {
                    val newLeftNode = new BTree[T](level, leafLevel, t.nodes.slice(0, t.nodes.length - 2))
                    val newRightNode = new BTree[T](level, leafLevel, t.nodes.slice(t.nodes.length - 1, t.nodes.length) ::: updatedNodesList.slice(index - 1, index).filter(_ != Nil) ::: newNode.nodes.filter(_ != Nil))
                    return new BTree[T](level, leafLevel, updatedNodesList.slice(0, index - 2) ::: newLeftNode :: t.nodes.slice(t.nodes.length - 2, t.nodes.length - 1) ::: newRightNode :: updatedNodesList.slice(index + 1, updatedNodesList.length))
                  }
                }
                case t: BTreeLeaf[T] =>
                case _ =>
              }
            }

            if (!rightNode.isEmpty && rightNode.head != Nil) {
              val node = rightNode.head
              node match {
                case t: BTree[T] => {
                  val newLeftNode = new BTree[T](level, leafLevel, newNode.nodes ::: updatedNodesList.slice(index + 1, index + 2) ::: removeLeftNil(t.nodes))
                  val newRootNodesList = updatedNodesList.slice(0, index) ::: newLeftNode :: updatedNodesList.slice(index + 3, updatedNodesList.length)
                  if (newRootNodesList.filter(_ != Nil).length.equals(1)) {
                    return newLeftNode
                  }
                  return new BTree[T](level, leafLevel, newRootNodesList)
                }
                case t: BTreeLeaf[T] => {
                  val newLeftNode = new BTree[T](level, leafLevel, newNode.nodes ::: updatedNodesList.slice(index + 1, index + 3))
                  val newRoot = new BTree[T](level, leafLevel, updatedNodesList.slice(0, index) ::: newLeftNode :: updatedNodesList.slice(index + 3, updatedNodesList.length))
                  if (newRoot.nodes.filter(_ != Nil).length.equals(1)) {
                    return newLeftNode
                  }
                  return newRoot
                }
                case _ =>
              }
            }

            if (!leftNode.isEmpty && leftNode.head != Nil) {
              val node = leftNode.head
              node match {
                case t: BTree[T] => {
                  val newLeftNode = new BTree[T](level, leafLevel, t.nodes ::: updatedNodesList.slice(index - 1, index) ::: removeLeftNil(newNode.nodes))
                  val newRootNodesList = updatedNodesList.slice(0, index - 2) ::: newLeftNode :: updatedNodesList.slice(index + 1, updatedNodesList.length)
                  if (newRootNodesList.filter(_ != Nil).length.equals(1)) {
                    return newLeftNode
                  }
                  return new BTree[T](level, leafLevel, newRootNodesList)
                }
                case t: BTreeLeaf[T] => {
                  val newRightNode = new BTree[T](level, leafLevel, updatedNodesList.slice(index + 1, index + 3).filter(_ != Nil) ::: newNode.nodes.filter(_ != Nil))
                  val newRoot = new BTree[T](level, leafLevel, updatedNodesList.slice(0, index - 2) ::: newRightNode :: updatedNodesList.slice(index + 1, updatedNodesList.length))
                  if (newRoot.nodes.filter(_ != Nil).length.equals(1)) {
                    return newRightNode
                  }
                  return newRoot
                }
                case _ =>
              }
            }
          }

          return new BTree[T](level, leafLevel, updatedNodesList)
        }
        case n: BTreeLeaf[T] => {
          val newLeaf = n.removeKey(key)

          if (newLeaf.elements.length.equals(0)) {
            if (!index.equals(0)) {
              return new BTree[T](level, leafLevel, nodes.slice(0, index - 1) ::: nodes.slice(index + 1, nodes.length))
            } else {
              return new BTree[T](level, leafLevel, Nil :: nodes.slice(1, nodes.length))
            }
          } else {
            if (newLeaf.elements.head.getKey > key) {
              if (!index.equals(0)) {
                return new BTree[T](level, leafLevel, nodes.slice(0, index - 1) ::: newLeaf.elements.head.getKey :: newLeaf :: nodes.slice(index + 1, nodes.length))
              } else {
                return new BTree[T](level, leafLevel, nodes.slice(0, index) ::: newLeaf :: nodes.slice(index + 1, nodes.length))
              }
            } else {
              return new BTree[T](level, leafLevel, nodes.slice(0, index) ::: newLeaf :: nodes.slice(index + 1, nodes.length))
            }
          }
        }
      }
    } else {
      throw new KeyNotFoundException("Key not found.")
    }
  }

  def removeLeftNil (elements: List[Any]): List[Any] = {
    val firstElement = elements.head

    firstElement match {
      case Nil => return elements.slice(2, elements.length)
      case _ => return elements
    }
  }
}

