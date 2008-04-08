package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import GraphAnalysis.{SCC => C, Vertex => V, Arc => A, _}


class GraphAnalysisTest {
  
  @Test def dependencies01(): Unit = {    
    val (a, b, c, d) = (V("a"), V("b"), V("c"), V("d"));
    val arcs = A(c, d) :: A(b, c) :: A(a, b) :: Nil
    val expected = C(Set(a), false) :: C(Set(b), false) :: C(Set(c), false) :: C(Set(d), false) :: Nil;
    val vs = List(a, b, c, d)
    val g = Graph(vs, arcs) 
    val actual = analizeDependencies(g)
    assertEquals(expected, actual)
  }
  
  @Test def dependencies02(): Unit = {    
    val (a, b, c, d) = (V("a"), V("b"), V("c"), V("d"));
    val expected = C(Set(a), true) :: C(Set(b), false) :: C(Set(c), false) :: C(Set(d), false) :: Nil;
    val arcs = A(a, a) :: A(c, d) :: A(b, c) :: A(a, b) :: Nil
    val vs = List(a, b, c, d)
    val g = Graph(vs, arcs) 
    val actual = analizeDependencies(g)
    assertEquals(expected, actual)
  }
  
  @Test def dependencies03(): Unit = {    
    val (a, b, c, d, e) = (V("a"), V("b"), V("c"), V("d"), V("d"));
    val expected = C(Set(a, b, c), true) :: C(Set(d, e), true) :: Nil;
    val arcs = A(a, b) :: A(b, c) :: A(c, a) :: A(d, e) :: A(e, d) :: A(c, d) :: A(b, e) :: Nil
    val vs = List(a, b, c, d, e)
    val g = Graph(vs, arcs) 
    val actual = analizeDependencies(g)
    assertEquals(expected, actual)
  }
  
}
