/**
 * Created by Mateusz Kleinert on 28.11.14.
 */
case class BTreeLeaf[T](leafLevel: Int, elementsList: List[BTreeElement[T]] = List()) extends BTreeNode {
  val elements: List[BTreeElement[T]] = elementsList

  def getFirstElementKey: Int = {
    elements.head.getKey
  }

  def printLeaf() = {
    for (e <- elements) {
      print(e.getKey + " ")
    }
  }

  def getValue(key: Int): T = {
    for (e <- elements) {
      if (e.getKey.equals(key)) {
        return e.getValue
      }
    }

    throw new KeyNotFoundException("Key not found.")
  }

  def isKey(key: Int): Boolean = {
    for (e <- elements) {
      if (e.getKey.equals(key)) {
        return true
      }
    }

    return false
  }

  def removeKey(key: Int): BTreeLeaf[T] = {
    for (e <- elements) {
      if (e.getKey.equals(key)) {
        val index = elements.indexOf(e)
        return new BTreeLeaf[T](leafLevel, elements.slice(0, index) ::: elements.slice(index + 1, elements.length))
      }
    }

    throw new KeyNotFoundException("Key not found.")
  }

  def addElement(newElement: BTreeElement[T]): List[BTreeLeaf[T]] = {
    if (elements.length.equals(0)) {
      return List(new BTreeLeaf[T](leafLevel, List(newElement)))
    } else if (elements.length < leafLevel) {
      for (element <- elements) {
        if (element.getKey >= newElement.getKey) {
          val index = elements.indexOf(element)
          return List(new BTreeLeaf[T](leafLevel, elements.slice(0, index) ::: newElement :: elements.slice(index, elements.length)))
        }

        return List(new BTreeLeaf[T](leafLevel, elements.slice(0, elements.length) ::: newElement :: List()))
      }
    } else {
      for (element <- elements) {
        if (element.getKey >= newElement.getKey) {
          val index = elements.indexOf(element)
          val newLeafElementsList: List[BTreeElement[T]] = elements.slice(0, index) ::: newElement :: elements.slice(index, elements.length)
          val middleElementIndex = newLeafElementsList.length / 2
          return List(new BTreeLeaf[T](leafLevel, newLeafElementsList.slice(0, middleElementIndex)), new BTreeLeaf[T](leafLevel, newLeafElementsList.slice(middleElementIndex, newLeafElementsList.length)))
        }
      }

      val newLeafElementsList: List[BTreeElement[T]] = elements.slice(0, elements.length) ::: newElement :: List()
      val middleElementIndex = newLeafElementsList.length / 2

      return List(new BTreeLeaf[T](leafLevel, newLeafElementsList.slice(0, middleElementIndex)), new BTreeLeaf[T](leafLevel, newLeafElementsList.slice(middleElementIndex, newLeafElementsList.length)))
    }

    throw new Exception()
  }
}
