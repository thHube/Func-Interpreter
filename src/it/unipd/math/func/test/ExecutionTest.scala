package it.unipd.math.func.test

import it.unipd.math.func.Executor

// -- Test if the execution of a function runs fine ----------------------------
object ExecutionTest {

  def execute(filename:String):Int = {
    var parseTree = ParsingTest.parse(filename);
    var executor:Executor = new Executor;
    
    // -- Normally should check ariety first.
    executor.execute(parseTree, List(1, 2));
  }
  
  def main(args:Array[String]) {
    println("1 + 2 = " + execute("examples/sum.f"));
  }
  
}