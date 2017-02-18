package poly.ai.lab2

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by nsokil on 13.02.2017.
  */

sealed case class Node(tag: String)

sealed case class Edge(nodeA: Node, nodeB: Node)

sealed class Graph {
  private var edges = Set[Edge]()
  private var neighbours = Map[Node, Set[Node]]()

  def addEdge(edge: Edge): Graph = {
    edges += edge
    var nA = if(neighbours.contains(edge.nodeA)) neighbours(edge.nodeA) else Set[Node]()
    nA += edge.nodeB
    var nB = if(neighbours.contains(edge.nodeB)) neighbours(edge.nodeB) else Set[Node]()
    nB += edge.nodeA
    neighbours += (edge.nodeA -> nA)
    neighbours += (edge.nodeB -> nB)
    this
  }
  def ++(edge: Edge): Graph = addEdge(edge)

  def dfs(startWith: Node): List[Node] = {
    @tailrec
    def dfsHelper(stack: Set[Node], visited: List[Node]): List[Node] = {
      if(stack.isEmpty) visited
      else dfsHelper(neighbours(stack.head).filterNot(visited.contains) ++ stack.tail, visited :+ stack.head)
    }
    dfsHelper(stack = Set(startWith), visited = Nil)
  }

  def bfs(startWith: Node): List[Node] = {
    val visited = mutable.ListBuffer(startWith)
    val queue = mutable.Queue[Node](startWith)
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      for(next <- neighbours(node))
        if(!visited.contains(next)) {
          visited += next
          queue.enqueue(next)
        }
    }
    visited.toList
  }
}

object Graph {
  def main(args: Array[String]) {
    val graph = new Graph()
    val a = Node("a"); val b = Node("b"); val c = Node("c")
    val d = Node("d"); val e = Node("e"); val f = Node("f")
    val g = Node("g"); val h = Node("h")
    graph ++ Edge(a, d) ++ Edge(d, c) ++ Edge(b, c) ++ Edge(d, b) ++ Edge(c, f)
    graph ++ Edge(f, e) ++ Edge(e, g) ++ Edge(c, g) ++ Edge(f, g) ++ Edge(c, h)
    println(graph.dfs(b))
    println(graph.bfs(b))
  }
}