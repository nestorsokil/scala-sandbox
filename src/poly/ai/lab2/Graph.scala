package poly.ai.lab2

import scala.collection.mutable

/**
  * Created by nsokil on 13.02.2017.
  */

sealed case class Node(tag: String)

sealed case class Edge(nodeA: Node, nodeB: Node)

sealed class Graph {
  private var edges = Set[Edge]()
  private var neighbours = Map[Node, Set[Node]]()

  def addEdge(edge: Edge): Unit = {
    edges += edge
    var nA = if(neighbours.contains(edge.nodeA)) neighbours(edge.nodeA) else Set[Node]()
    nA += edge.nodeB
    var nB = if(neighbours.contains(edge.nodeB)) neighbours(edge.nodeB) else Set[Node]()
    nA += edge.nodeA
    neighbours += (edge.nodeA -> nA)
    neighbours += (edge.nodeB -> nB)
  }
  def ++(edge: Edge): Unit = addEdge(edge)

  def breadthFirstSearch(startWith: Node, tag: String): Boolean = {
    breadthFirstSearch(startWith, Node(tag))
  }


  def depthFirstSearch(startWith: Node, toFind: Node): Boolean = {
    var visited = Set[Node]()
    val stack = mutable.Stack[Node]()
    stack.push(startWith)
    while (stack.nonEmpty){
      val node = stack.pop()
      if(node == toFind) return true
      for(next <- neighbours(node)){
        if(!visited.contains(next)){
          visited += next
          stack.push(next)
        }
      }
    }
    false
  }

  def breadthFirstSearch(startWith: Node, toFind: Node): Boolean = {
    var visited = Set[Node]()
    val queue = mutable.Queue[Node]()
    queue += startWith
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      if(node == toFind) return true
      for(next <- neighbours(node)) {
        if(!visited.contains(next)){
          visited += next
          queue.enqueue(next)
        }
      }
    }
    false
  }
}
