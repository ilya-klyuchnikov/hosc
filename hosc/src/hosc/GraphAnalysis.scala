package hosc

object GraphAnalysis {
  case class Vertex(val name: String){
    var visited = false
    var number = 0
  }
  
  case class Arc(from: Vertex, to: Vertex){
    var visited = false
    override def toString = from.name + "->" + to.name
  }
  case class Graph(v: List[Vertex], e: List[Arc])
  
  case class SCC(var vs: Set[Vertex], resursive: Boolean)
  
  private def dfs1(g: Graph) = {
    var n = 0
    def dfs1_(v: Vertex): Unit = {
      if (!v.visited) {
        v.visited = true
        for (a <- g.e filter {_.from == v}) dfs1_(a.to)
        n += 1
        v.number = n
      }
    }
    while (g.v exists (_.visited == false)){
      val v = (g.v find (_.visited == false)).get
      dfs1_(v)
    }
  }
  
  private def dfs(g: Graph) = {
    var n = 0
    def dfs_(v: Vertex): Unit = {
      if (!v.visited) {
        v.visited = true
        n += 1
        v.number = n
        for (a <- g.e filter {_.from == v}) {
          if (!a.to.visited) a.visited = true
          dfs_(a.to)
        }
      }
    }
    while (g.v exists (_.visited == false)){
      val v = (g.v find (_.visited == false)).get
      dfs_(v)
    }
  }
  
  private def discoverSCC(g: Graph) = {    
    dfs1(g)
    // invert g
    val vs = (g.v sort {(v1, v2) => v1.number > v2.number}) map {v => Vertex(v.name)}
    var e = g.e map {a => Arc(vs.find(_.name == a.to.name).get, vs.find(_.name == a.from.name).get)}
    val gr = Graph(vs, e)
    dfs(gr)
    
    var sccs = List[SCC]()
    for (a <- gr.e.filter {_.visited}) {
      sccs find {s => (s.vs contains a.from) || (s.vs contains a.to)} match {
        case Some(scc) => scc.vs = scc.vs + a.from + a.to
        case None => sccs = SCC(Set(a.from, a.to), true) :: sccs
      }
    }
    
    // add singletons
    for (v <- gr.v) sccs find (_.vs contains v) match {
      case None => sccs = SCC(Set(v), g.e contains Arc(v, v)) :: sccs
      case _ => 
    }
    
    sccs
  }
  
  private def topSort(sccs: List[SCC], g: Graph) = {
    def less(scc1: SCC, scc2: SCC) = {
      g.e exists {a => (scc1.vs contains a.from) && (scc2.vs contains a.to)}
    }
    sccs sort less
  }
  
  def analizeDependencies(g: Graph) = {
    topSort(discoverSCC(g), g)
  }
  
}
