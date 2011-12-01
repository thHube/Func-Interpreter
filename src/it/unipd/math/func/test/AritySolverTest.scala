package it.unipd.math.func.test

import it.unipd.math.func.AritySolver

object AritySolverTest {

  def solveArity(filename:String):Int = {
    var parseTree = ParsingTest.parse(filename);
    var solver:AritySolver = new AritySolver;
    
    solver.check(parseTree);
  }
  
  def main(args:Array[String]) {
    println("Got it! Arity is " + solveArity("examples/sum.f"));
  }
  
}