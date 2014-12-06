# Scala1

Generic B-Tree implemented in Scala programming language using recursion. 

Used classes:
 - BTreeNode - abstract class
 - BTree[T] - implements node of a tree. Constructor's arguments:
   - level: Int = 2  # Indicates how many child nodes a node may possess
   - leafLevel: Int = 3 # Indicates how many elements a leaf may store 
   - nodesList: List[Any] = List() # Indicates a node structure in a given manner: node | key | node | key ...
 - BTreeElement[T] - implements a single element. Constructor's arguments:
   - value: T 
   - key: Int
 - BTreeLeaf[T] - A leaf of a tree. Special node that stores only elements of class BTreeElement[T]. Constructor's arguments:
   - leafLevel: Int # Indicates how many elements a leaf may store
   - elementsList: List[BTreeElement[T]] = List() 
 - KeyNotFoundException

Available functions:
 - DFSTraversal (debug: Boolean = false)
 - BFSTraversal (debug: Boolean = false)
 - addElement (value: T, key: Int): BTree[T]
 - isKey (key: Int): Boolean
 - getValue (key: Int): T
 - removeKey (key: Int): BTree[T]

### Version
1.0.0
