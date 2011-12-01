package it.unipd.math.func

import scala.io.Source

// -- This object defines behaviour forinteracive shell of interpreter ---------
object InteractiveShell { 

  val SHELL_VERSION:String = "0.1";
  val SHELL_CURSOR = ">> "
  
  // -- Compiler core componets ------------------------------------------------
  var lexer:Lexer = new Lexer;
  var parser:Parser = new Parser;
  var paramPrs:ParamParser = new ParamParser;
  var aritySolver:AritySolver = new AritySolver;
  var executor:Executor = new Executor;
  
  // -- Current function stab and param request --------------------------------
  var current:Node = null;
  var readParam:Boolean = false;
  
  def parseLine(line:String) = line match {
    // -- Help message and quit command ----------------------------------------
    case "help" => printHelpMessage();
    case "quit" => System.exit(0);
    
    // -- All other cases ------------------------------------------------------
    case _=> {
      if (readParam) {
        val tokens:List[Token] = lexer.lex(line.toList);
        val params:List[Int] = paramPrs.parse(tokens);
        val arity:Int = aritySolver.check(current);
        if (params.size != arity) {
          println("Mismatch arity and param count.");
          println("required: " + arity + " - found: " + params.size);
        } else {
          val result = executor.execute(current, params);
          println("Result = " + result);
          readParam = false;
        }
      } else {
        val tokens = lexer.lex(line.toList);
        current = parser.parse(tokens);
        var arity = aritySolver.check(current);
        // -- Now we expect params ---------------------------------------------
        readParam = true;
        
        // -- Print operation success ------------------------------------------
        println("Read: " + line);
        println("Done, waiting for " + arity + " param(s)");
      }
    }
  }
  
  // -- Print an hello message once interactive shell is started ---------------
  def printHelloMessage() {
    println("Func interactive shell v" + SHELL_VERSION);
    println("created by Alberto Franco (c) 2011");
    println("Type 'help' for commands, 'quit' for closing the shell.");
  }
  
  def printHelpMessage() {
    println("Commands: ");
    println("  help: print this message")
    println("  quit: close the program");
    println("Syntax: ");
    println("  Z, S, P(n, i)     - Zero, successor and project i on n functions ");
    println("  C[F, G1, G2, ...] - compose F(G1(), G2(), ...)");
    println("  R[K, G]           - Recursion, K is base and G recursive cases");
    println("  M[G]              - Minimum x s.t. G is zero");
    println("  (p1, p2, ....)    - To pass params to functions");
  }
  
  // --  Program entry point ---------------------------------------------------
  def main(args:Array[String]) {
    printHelloMessage();
    print(SHELL_CURSOR);
    for (line <- Source.stdin.getLines()) {
      try {
    	parseLine(line);
      } catch {
        case e:Exception => {
          println("-- Operation aborted")
          e.printStackTrace;
        }
      } finally {
        print(SHELL_CURSOR);
      }
    }
  }
  
}