package hosc.spec

import org.junit.{Test, Ignore}
import org.junit.Assert._

import hosc.Util._
import hosc.TestUtils._
import hosc.HLanguage._
import hosc.{SuperCompiler0, CodeConstructor, Eq}

class EqualitySpec {
  
  val in = "lists"
  
  @Test def appendR =
    testEq(in, 
           "appendR (appendR xs ys) zs", 
           "appendR xs (appendR ys zs)")
  
  @Test def appendL =
    testEq(in, 
           "appendL (appendL xs ys) zs", 
           "appendL xs (appendL ys zs)")
  
  @Test def appendR1 =
    testEq(in, 
           "appendR xs ys", 
           "foldR ys cons xs")
  
  @Test def appendL1 =
    testEq(in, 
           "appendL xs ys", 
           "foldL xs snoc ys")
  
  @Test def appendR2 =
    testEq(in, 
           "appendR xs ys", 
           """foldR id (\x f a -> cons x (f a)) xs ys""")
  
  @Test def appendL2 =
    testEq(in, 
           "appendL xs ys", 
           """foldL id (\f x a -> snoc (f a) x) ys xs""")
  
  @Test def appendL3 =
    testEq(in, 
           "appendL xs ys", 
           """foldL1 snoc xs ys""")

  // free and bound f 
  @Ignore 
  @Test def bug =
    testEq(in, 
           "appendL xs ys", 
           """foldL (foldL id) f xs ys""")
  
  @Test def map =
    testEq(in, 
           """listR f xs""", 
           """foldR NilR (\a x -> Cons (f a) x) xs""")
  
  @Test def concat =
    testEq(in, 
           "concatR xs", 
           "foldR NilR catR xs")
  
  @Test def length1 =
    testEq(in, 
           """lengthR xs""", 
           """foldR Z (\a n -> (S n)) xs""")
  
  @Test def length2 =
    testEq(in, 
           """lengthR xs""", 
           """(compose sum (listR (\x -> S Z))) xs""")
  
  @Test def convert =
    testEq(in, 
           """convertL2R xs""", 
           """convertL2R xs""")
  
  @Test def filter1 =
    testEq(in, 
           """filterR p xs""", 
           """(compose concatR (listR (cond p wrapR nilpR))) xs""")
  
  @Test def filter2 =
    testEq(in, 
           """filterR p xs""", 
           """(compose (foldR NilR catR) (listR (cond p wrapR nilpR))) xs""")
  
  @Test def filter3 =
    testEq(in, 
           """filterR p xs""", 
           """(compose (foldR NilR catR) (foldR NilR (\a x -> Cons ((cond p wrapR nilpR) a) x))) xs""")
  
  @Test def filter4 =
    testEq(in, 
           """filterR p xs""", 
           """foldR1 NilR (cond (compose p outl) (uncurry cons) outr) xs""")
  
  @Test def filter5 =
    testEq(in, 
           """filterR p xs""", 
           """foldR NilR (curry (cond (compose p outl) (uncurry cons) outr)) xs""")
  
  @Test def plus1 =
    testEq(in, 
           """plus x y""", 
           """foldN x succ y""")
  
  @Test def plus2 =
    testEq(in, 
           """plus1 x y""", 
           """foldN y succ x""")
  
  @Test def mult1 =
    testEq(in, 
           """mult x y""", 
           """foldN Z (plus x) y""")
  
  @Test def mult2 =
    testEq(in, 
           """mult1 x y""", 
           """foldN Z (plus y) x""")
  
  @Test def mult3 =
    testEq(in, 
           """mult1 x y""", 
           """foldN Z (\n -> plus1 n y) x""")
  
  @Ignore
  @Test def unzip1 =
    testEq(in, 
           """unzipR xs""", 
           """unzipR1 xs""")
  
  @Ignore
  @Test def unzip2 =
    testEq(in, 
           """xs""", 
           """uncurry zipR (unzipR xs)""")
  
  @Test def map_concat =
    testEq(in, 
           """listR f (concatR xs)""", 
           """concatR (listR (listR f) xs)""")
  
  @Test def inits =
    testEq(in, 
           """listL (listL f) (inits xs)""", 
           """inits (listL f xs)""")
  
  @Ignore
  @Test def inits1 =
    testEq(in, 
           """listL (listL f) (initsL xs)""", 
           """initsL (listL f xs)""")
  
  @Test def zip =
    testEq(in, 
           """listR (cross (P f g)) (zipR xs ys) """, 
           """uncurry zipR (cross (P (listR f) (listR g)) (P xs ys))""")
  
  @Test def zip_filter =
    testEq(in, 
           """filterR p xs""", 
           """listR outl (filterR outr (uncurry zipR (pair (P id (listR p)) xs)))""")
  
  
  val in1 = "flists"
  
  @Test def rev1 =
    testEq(in1, 
           """rev xs""", 
           """foldr Nil append xs""")
  
  @Test def rev2 =
    testEq(in1, 
           """rev1 xs Nil""", 
           """foldl Nil prepend xs""")
  
  @Test def rev3 =
    testEq(in1, 
           """map f (rev1 (Cons a Nil) (Cons b Nil))""", 
           """rev1 (map f (Cons a Nil)) (map f (Cons b Nil))""")
  
  @Test def rev4 =
    testEq(in1, 
           """map f (rev1 (Cons a Nil) ys)""", 
           """rev1 (map f (Cons a Nil)) (map f ys)""")
  
  @Ignore
  @Test def rev5 =
    testEq(in1, 
           """map f (rev1 xs ys)""", 
           """rev1 (map f xs) (map f ys)""")
  
  
  

  def testEq(input: String, goal1: String, goal2: String): Unit = {
    
    val file = "spec_eq/" + input + ".hs"
    val program = programFromFile(file)
    val input1 = termFromString(goal1, program)
    val input2 = termFromString(goal2, program)
    val p1 = sc(program, input1)
    val p2 = sc(program, input2)
    println("===================")
    println(goal1 + " == " + goal2)
    println(p1.toDocString)
    println(p2.toDocString)
    assertTrue(goal1 + " and " + goal2 + " should be equivalent", Eq.equivalent(p1.goal, p2.goal))
  }
  
  def sc(program: Program, goal: Expression) = {
    val sc0 = new SuperCompiler0(program)
    val pt = sc0.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}
