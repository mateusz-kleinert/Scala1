/**
 * Created by Mateusz Kleinert on 03.12.14.
 */
object Main extends App {
  var tree = new BTree[String](3, 2)
  tree = tree.addElement("four", 4)
  tree = tree.addElement("two", 2)
  tree = tree.addElement("one", 1)
  tree = tree.addElement("seven", 7)
  tree = tree.addElement("eight", 8)
  tree = tree.addElement("three", 3)
  tree = tree.addElement("five", 5)
  tree = tree.addElement("six", 6)

  tree.DFSTraversal()
  println()

  tree = tree.removeKey(4)
  tree = tree.removeKey(5)
  tree = tree.removeKey(7)

  tree.DFSTraversal()
  println()

  tree = tree.removeKey(6)

  tree.DFSTraversal()
  println()

}
