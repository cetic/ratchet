package be.cetic.ratchet.reader.ast

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

import be.cetic.ratchet.reader.helpers.{ASTException, NoFatherException, Visitable}

trait printHelpers {
  def Spaces(n: Int): String = if (n <= 0) "" else " " + Spaces(n - 1)

  def SpacesAsHead(indent: Int, head: Boolean): String = (if (head) Spaces(indent) else "")
}

trait AstElement

trait PropertyMap {
  private var properties: Map[Any, Any] = Map[Any, Any]()

  def clearProperties = properties = Map[Any, Any]()

  def addProperties(key: Any, value: Any) = properties = properties + (key -> value)

  def getProperties(key: Any): Option[Any] = properties.get(key)
}


//TODO: ajouter un trait ASTReference pour les références entre noeuds (sans be.cetic.ratchet.transformer l'AST En DAG)
/*
trait ASTReference{

def referenced:ReferencedAstNode

referenced.registerReference(this)
}

et ajouter un trait à AstNode:
ReferencedAstNode{
  referencing:List[AstReference]
}

 */


object IdentifiableIdManager {
  private var id = 0L

  def createID(): Long = {
    id = id + 1; id
  }
}

trait Identifiable {
  private val id = IdentifiableIdManager.createID()

  def exactlySame(other: Identifiable) = other.id == this.id
}


/** represents a node in the parse tree.
  * AST nodes are immutable, that is; their immediate children cannot be changed.
  * if they implement the replaceChild, they become mutable.
  * ASTLists also can be modified by the insertAfter and insertBefore
  */
trait AstNode extends printHelpers with DistributedStorageUtility with AstElement with Cloneable with PropertyMap with Visitable with Identifiable {

  def getStorageManager: StorageUtilityManager = {
    enclosingOrThis[StorageUtilityManager with AstNode] match {
      case None => throw new Error("no StorageUtilityManager found")
      case Some(x) => x
    }
  }

  private var _tree: AstTree = null

  def tree = _tree

  def tree_=(t: AstTree) {
    _tree = t
  }

  def nodeinfo = "TYPE=" + this.getClass.getCanonicalName + ",VALUE=" + this.toString

  def checkFather = if (father == null) throw NoFatherException("Father is null. operation impossible without father. node type" + nodeinfo)

  def replaceBy(by: AstNode) {
    checkFather
    father.replaceChild(this, by)
  }

  final def insertBefore(nodes: AstNode*) {
    checkFather; for (node <- nodes) {
      father.insertBeforeChild(this, node); node.checkFather
    }
  }

  /** inserted node in the given order */
  final def insertAfter(nodes: AstNode*) {
    checkFather
    var lastInserted = this
    for (node <- nodes) {
      father.insertAfterChild(lastInserted, node)
      node.checkFather
      lastInserted = node
    }
  }

  final def delete() {
    checkFather; father.deleteChild(this)
  }

  private var _father: AstNode = null

  //a node can only have a single father.
  //I'd like to have a better type than this crap.
  final def father: AstNode = _father

  final def father_=(n: AstNode) {
    if (tree != null) {
      tree.removeMe(this)
    }
    _father = n
    if (n != null && n.tree != null) {
      n.tree.insertMe(this)
    }
  }


  def enclosingOrThis[T <: AstNode](implicit m: Manifest[T]): Option[T] = {
    this match {
      case t if m.runtimeClass.isInstance(t) => Some(t.asInstanceOf[T])
      case _ => enclosing[T]
    }
  }

  final def enclosing[T <: AstNode](implicit m: Manifest[T]): Option[T] = {
    this.father match {
      case null => None
      case t if m.runtimeClass.isInstance(t) => Some(t.asInstanceOf[T])
      case x => x.enclosing[T]
    }
  }

  final def enclosingOrDie[T <: AstNode](implicit m: Manifest[T]): T = {
    this.enclosing[T](m) match {
      case Some(x) => x
      case None => throw new Exception("No enclosing node of type " +
        m.runtimeClass.getName + " for  " + this.getClass.getName)
    }
  }

  def replaceChild(child: AstNode, by: AstNode) {
    throw ASTException("replaceChild not implemented for " + this.getClass.getName)
  }

  def insertBeforeChild(child: AstNode, node: AstNode) {
    throw ASTException("insertBeforeChild not implemented for " + this.getClass.getName)
  }

  def insertAfterChild(child: AstNode, node: AstNode) {
    throw ASTException("insertAfterChild not implemented for " + this.getClass.getName)
  }

  def deleteChild(child: AstNode) {
    throw ASTException("deleteChild not implemented for " + this.getClass.getName)
  }

  def children: List[AstNode]

  final def descendants(maxDepth: Int = Integer.MAX_VALUE): List[AstNode] = this.getDFS(maxDepth)

  final def descendantsOfType[T <: AstNode](maxDepth: Int = Integer.MAX_VALUE)(implicit m: Manifest[T]) =
    descendants(maxDepth).filter(m.runtimeClass.isInstance(_)).asInstanceOf[List[T]]

  final def firstDescendantsOfType[T <: AstNode](maxDepth: Int = Integer.MAX_VALUE)(implicit m: Manifest[T]): Option[T] = {
    for (child <- children) {
      if (m.runtimeClass.isInstance(child)) {
        return Some(child.asInstanceOf[T])
      }
      else child.firstDescendantsOfType[T](maxDepth - 1)(m) match {
        case None => //nothing next children
        case Some(x) => return Some(x.asInstanceOf[T])
      }
    }
    None
  }

  final def haveDescendantsOfType[T <: AstNode](maxDepth: Int = Integer.MAX_VALUE)(implicit m: Manifest[T]): Boolean
  = firstDescendantsOfType[T](maxDepth)(m).isDefined

  final def isFollowedByType[T <: AstNode](implicit m: Manifest[T]): Boolean = nextSibling match {
    case None => false
    case Some(t) => m.runtimeClass.isInstance(t)
  }

  final def nextSibling = {
    checkFather; father.nextChild(this)
  }

  final def previousSibling = {
    checkFather; father.previousChild(this)
  }

  final def nextSiblingOfType[T](implicit m: Manifest[T]): Option[T] = {
    this.nextSibling match {
      case Some(s) if m.runtimeClass.isInstance(s) => Some(s.asInstanceOf[T])
      case Some(s) => s.nextSiblingOfType[T](m)
      case None => None
    }
  }

  final def previousSiblingOfType[T](implicit m: Manifest[T]): Option[T] = {
    this.previousSibling match {
      case Some(s) if m.runtimeClass.isInstance(s) => Some(s.asInstanceOf[T])
      case Some(s) => s.previousSiblingOfType[T](m)
      case None => None
    }
  }

  final def nextChild(child: AstNode): Option[AstNode] = {
    val index = children.indexOf(child) + 1
    if (index > 0 && index < children.size) Some(children(index))
    else None
  }

  final def previousChild(child: AstNode): Option[AstNode] = {
    val index = children.indexOf(child) - 1
    if (index >= 0) Some(children(index))
    else None
  }

  final def nextChildren(child: AstNode) = children.filter(c => children.indexOf(c) > children.indexOf(child))

  final def previousChildren(child: AstNode) = children.filter(c => children.indexOf(c) < children.indexOf(child)).reverse

  final def nextChildrenOfType[T](child: AstNode)(implicit m: Manifest[T]) = nextChildren(child).filter(x => m.runtimeClass.isInstance(x)).asInstanceOf[List[T]]

  final def previousChildrenOfType[T](child: AstNode)(implicit m: Manifest[T]) = previousChildren(child).filter(x => m.runtimeClass.isInstance(x)).asInstanceOf[List[T]]

  final def childrenOfType[T](implicit m: Manifest[T]): List[AstNode] = children.filter(x => m.runtimeClass.isInstance(x))

  /** returns a DFS starting and including from this node */
  final def getDFS(maxDepth: Int = Integer.MAX_VALUE): List[AstNode] = prependDFS(List.empty, maxDepth)

  final def getSpecialBFS(prepender: (AstNode) => List[AstNode], stop: (AstNode) => Boolean): Option[AstNode] = {
    var nodes = List[AstNode](this)
    var current: AstNode = null

    while (!nodes.isEmpty) {
      current = nodes.head
      nodes = nodes.tail

      if (stop(current)) current
      else nodes = nodes ::: prepender(current)
    }

    None
  }

  final def prependDFS(Tail: List[AstNode], maxDepth: Int = Integer.MAX_VALUE): List[AstNode] =
    if (maxDepth == 0)
      Tail
    else this :: children.foldRight(Tail)((node: AstNode, tail: List[AstNode]) => node.prependDFS(tail, maxDepth - 1))


  def maxDepthOfType[T <: AstNode](implicit m: Manifest[T]): Int = {
    if (!this.isInstanceOf[T]) 0
    else {
      val children = childrenOfType[T](m)
      if (children.size == 0) 0
      else childrenOfType[T](m).map(c => c.maxDepthOfType[T](m)).reduce((a, b) => math.max(a, b))
    }
  }

  def sameTree(other: AstNode): Boolean = {
    def sameChildren(n: AstNode) = (this.children.size == n.children.size) &&
      (children.zip(n.children).filterNot(x => x._1.sameTree(x._2)).size == 0)

    other.getClass.getName.eq(this.getClass.getName) && sameChildren(other)
  }

  override def equals(obj: Any): Boolean = obj match {
    case t: AstNode => exactlySame(t)
    case _ => false
  }

  def path: String = (if (father == null) "[root]" else father.path) + "/" + this.getClass.getName
}

trait terminalASTNode extends AstNode {
  override def children: List[AstNode] = List.empty

}

trait NoDelete[T <: AstNode] extends ASTList[T] {
  override def deleted(listBefore: List[T], node: T, listAfter: List[T]) {
    throw ASTException("Cannot delete " + node.getClass.getName + " from " + this.getClass.getName)
  }
}

trait ASTList[T <: AstNode] extends AstNode with Visitable {
  def list: List[T]

  def list_=(a: List[T])

  override def toString: String = list.mkString("\n")

  /** appends an ASTNode to the List, same to insert after last, actually
    *
    * @param a the ASTNode to be appended
    */
  final def append(a: AstNode) {
    val aAsT = a.asInstanceOf[T]
    inserted(list, aAsT, List.empty)
    list = list ::: List(aAsT)
    a.father = this
  }

  final def append(nodes: AstNode*) {
    for (node <- nodes) append(node)
  }

  final def prepend(a: AstNode) {
    val aAsT = a.asInstanceOf[T]
    inserted(List.empty, aAsT, list)
    list = aAsT :: list
    a.father = this
  }

  final def prepend(nodes: AstNode*) {
    for (node <- nodes.reverse) prepend(node)
  }

  final override def children: List[AstNode] = list

  /** Called when some replacement was performed
    * the default is to call deleted followed byt inserted */
  def replaced(listBefore: List[T], what: T, by: T, listAfter: List[T]) {
    deleted(listBefore, what, listAfter)
    inserted(listBefore, by, listAfter)
  }

  final override def replaceChild(child: AstNode, by: AstNode) {
    val byAsT: T = by.asInstanceOf[T]
    var listBefore: List[T] = List.empty
    var listAfter: List[T] = List.empty

    def doit(from: List[T]): List[T] = from match {
      case head :: tail =>
        if (head == child) {
          listAfter = tail; byAsT :: tail
        }
        else {
          listBefore = head :: listBefore; head :: doit(tail)
        }
      case nil => throw ASTException("cannot find node " + child + " in " + this)
    }

    list = doit(list)
    if (child.father == this) child.father = null
    by.father = this
    replaced(listBefore.reverse, child.asInstanceOf[T], byAsT, listAfter)
  }

  /** Called when some node was inserted
    * no assurance of conformance with the list at this point */
  def inserted(listBefore: List[T], node: T, listAfter: List[T]) {}

  final override def insertBeforeChild(child: AstNode, node: AstNode) {
    val nodeAsT: T = node.asInstanceOf[T]
    var listBefore: List[T] = List.empty
    var listAfter: List[T] = List.empty

    def doit(from: List[T]): List[T] = from match {
      case head :: tail =>
        if (head == child) {
          listAfter = from; nodeAsT :: from
        }
        else {
          listBefore = head :: listBefore; head :: doit(tail)
        }
      case nil => throw ASTException("cannot find node " + child + " in " + this)
    }

    list = doit(list)
    node.father = this
    inserted(listBefore.reverse, nodeAsT, listAfter)
  }

  final override def insertAfterChild(child: AstNode, node: AstNode) {
    val nodeAsT: T = node.asInstanceOf[T]
    var listBefore: List[T] = List.empty
    var listAfter: List[T] = List.empty

    def doit(from: List[T]): List[T] = from match {
      case head :: tail =>
        if (head == child) {
          listAfter = tail; listBefore = List(head); head :: nodeAsT :: tail
        }
        else {
          listBefore = head :: listBefore; head :: doit(tail)
        }
      case nil => throw ASTException("cannot find node " + child + " in " + this)
    }

    list = doit(list)
    node.father = this
    inserted(listBefore.reverse, nodeAsT, listAfter)
  }

  /** Called when some replacement was performed */
  def deleted(listBefore: List[T], node: T, listAfter: List[T]) {}

  final override def deleteChild(deletedN: AstNode) {
    var listBefore: List[T] = List.empty
    var listAfter: List[T] = List.empty

    def doit(from: List[T]): List[T] = from match {
      case head :: tail =>
        if (head == deletedN) {
          listAfter = tail; tail
        }
        else {
          listBefore = head :: listBefore; head :: doit(tail)
        }
      case nil => throw ASTException("cannot find node " + deletedN + " in " + this)
    }

    list = doit(list)
    deletedN.father = null;
    deleted(listBefore.reverse, deletedN.asInstanceOf[T], listAfter)
  }

  /** call this if you want all elements in the list to be notified for insert, typically at startup */
  final def notifyAllInserts() {

    var listBefore: List[T] = List.empty

    for (i <- list) {
      inserted(listBefore, i, List.empty)
      listBefore = listBefore ::: List(i)
      i.father = this
    }
  }
}


