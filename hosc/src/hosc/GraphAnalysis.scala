package hosc

/** 
  *  GraphAnalysis module incorporates algorithms for splitting graph into graph components
  *  (namely: strongly connected components and singleton components) and for performing 
  *  topological sorting of mentioned components.
  *  
  *  This module is used by TypeInferrer to perform dependency analysis of functions in
  *  program.
  *
  *  For algorithm details see:
  *  Aho, A.V., Hopcroft, J.E. and Ullman, D. 1983, Data structures and algorithms, pp. 221-226
  */
object GraphAnalysis {
  
  case class Vertex(name: String){
    var visited = false
    var number = 0
  }
  
  case class Arc(from: Vertex, to: Vertex){
    var visited = false
    override def toString = from.name + "->" + to.name
  }
  
  case class Graph(vertices: List[Vertex], arcs: List[Arc])
  
  /** 
   *  A graph component is either a strongly connected component or a singleton. 
   *
   *  A strongly connected component of a directed graph is a maximal set of vertices in
   *  which there is a path from any one vertex in the set to any other vertex in the set.
   *  If GraphComponent is strongly connected component then recursive = true.
   *  
   *  A singleton is a vertex of a graph that doesn't belong to any 
   *  strongly connected component. If GraphComponent is a singleton then vs consists of 
   *  exactly one element and recursive = false.
   */
  case class GraphComponent(var vs: Set[Vertex], recursive: Boolean)
  
  /** 
   *  Performs a depth-first search of a given graph and numbers the vertices in order of
   *  completion of the recursive calls. That is, a vertex is numbered after all adjacent 
   *  vertices are traversed. Also marks traversed arcs as visited. An arc from a vertex
   *  to itself is marked as visited also.
   *
   *  Firstly, the first vertex from graph.vertices is taken and is passed
   *  as an argument to traverse procedure. After traverse procedure 
   *  completes, the next vertex that was not yet visited is passed as an argument 
   *  to traverse procedure until there are no not visited vertices.
   *
   *  The traversal order depends on an order of graph.vertices.
   *
   *  This method changes the state of a given graph. 
   *
   *  @param graph the graph to be traversed
   */
  private def depthFirstTraverse(g: Graph): Unit = {
    var n = 0
    
    def traverse(v: Vertex): Unit = {
      if (!v.visited) {
        v.visited = true
        for (a <- g.arcs filter {_.from == v}) {
          if (a.to == v || !a.to.visited)
            a.visited = true
          traverse(a.to)
        }
        n += 1
        v.number = n
      }
    }
    
    for (v <- g.vertices) traverse(v)
  }
  
  /** 
   *  Constructs a new directed graph by reversing the direction of every arc in a given graph.
   *  Given graph should be already traversed. That is, every vertex should have unique n greater than 0.
   *  Vertices in resulted graph are sorted in descending order by vertex number in a given graph.
   *
   *  @param  graph the graph to be reversed. Given graph should be already traversed.
   *  @return new graph where arcs are reverted and vertices are sorted in descending order by vertex number 
   *          in traversed graph.
   */
  private def reverseTraversedGraph(graph: Graph): Graph = {
    val vertices = (graph.vertices sort {(v1, v2) => v1.number > v2.number}) map {v => Vertex(v.name)}
    val vsMap = Map(vertices map {v => (v.name, v)}:_*)
    val arcs = graph.arcs map {a => Arc(vsMap(a.to.name), vsMap(a.from.name))}
    Graph(vertices, arcs)
  }
  
  /** 
   *  Finds components of this graph.
   *  
   *  Algorithm to find the components of a given directed graph we use:
   *
   *  1. Perform a depth-first traversal of a given graph.
   *  2. Construct a new directed graph reversedGraph by reversing the direction of every arc in
   *     a given graph.
   *  3. Perform a depth-first traversal of reversedGraph starting the search from the highest-
   *     numbered vertex according to the numbering assigned at step (1). If the depth-
   *     first traversal does not reach all vertices, start the next depth-first traversal from
   *     the highest-numbered remaining vertex.
   *  4. Each tree in the resulting spanning forest is a strongly connected component of graph.
   *     Construct strongly connected components and singletons components.
   *
   *  @param  graph the graph to be splitted into components 
   *  @return a list of graph components
   */  
  private def findComponents(graph: Graph): List[GraphComponent] = {    
    var components = List[GraphComponent]()
    
    // 1.
    depthFirstTraverse(graph)
    // 2.
    val reversedGraph = reverseTraversedGraph(graph)
    // 3.
    depthFirstTraverse(reversedGraph)   
    
    // 4.a construct strongly connected components
    for (a <- reversedGraph.arcs.filter {_.visited}) {
      components partition {comp => (comp.vs contains a.from) || (comp.vs contains a.to)} match {
        case (Nil, l2) => components = GraphComponent(Set(a.from, a.to), true) :: l2
        case (l1, l2)  => components = GraphComponent(l1.foldLeft(Set(a.from, a.to)){_ ++ _.vs}, true) :: l2
      }
    }
    
    // 4.b construct singleton components
    for (v <- reversedGraph.vertices) components find (_.vs contains v) match {
      case None => components = GraphComponent(Set(v), false) :: components
      case _ => 
    }
    
    components
  }
  
  /** 
   *  Performs a revert topological sorting on graph components.
   *
   *  Revert topological ordering is a linear ordering of components in which 
   *  each component comes after all components to which it has outbound edges.
   *
   *  @param graph the graph to be traversed
   */  
  private def topologicalSort(components: List[GraphComponent], g: Graph): List[GraphComponent] = {
    def findIndependentComponent(cs: List[GraphComponent]): GraphComponent = {
      (cs find 
       {s1 => cs forall { s2 => s1 == s2 || !(g.arcs exists {a => (s2.vs contains a.from) && (s1.vs contains a.to)})} }).get
    }
    
    components match {
      case Nil => Nil;
      case _ => {
        val ind = findIndependentComponent(components)
        val others = components remove {_ == ind}
        ind :: topologicalSort(others, g)
      }
    }
  }
  
  /**
   *  Finds components of a given graph and then sort them in revert topological order.
   *
   *  @param    g the graph to be analyzed
   *  @return   strong components of graph sorted in revert topological order
   */
  def analyzeDependencies(g: Graph): List[GraphComponent] = {
    topologicalSort(findComponents(g), g)
  }
  
}