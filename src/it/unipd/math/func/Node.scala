package it.unipd.math.func

// -- A node of the parse tree -------------------------------------------------
abstract class Node {
  // -- Every function has his arity, it's used for checking the program. -----
  private var arity:Int = 1;
  
  // -- Setter and getter. ----------------------------------------------------- 
  def setArity(newVal:Int) = { arity = newVal; }
  def getArity():Int = arity; 
}

// -- Zero function to apply ---------------------------------------------------
case object Zero extends Node

// -- Successor ----------------------------------------------------------------
case object Succ extends Node

// -- Projection ---------------------------------------------------------------
case class Proj(arity:Int, on:Int) extends Node

// -- Function composition -----------------------------------------------------
case class Cons(fst:Node, comp:List[Node]) extends Node

// -- Recursion ----------------------------------------------------------------
case class Rec(base:Node, induc:Node) extends Node

// -- Minimization -------------------------------------------------------------
case class Min(prog:Node) extends Node
