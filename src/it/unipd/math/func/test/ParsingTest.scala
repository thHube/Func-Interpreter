package it.unipd.math.func.test

import scala.io.Source

import it.unipd.math.func.Lexer
import it.unipd.math.func.Parser
import it.unipd.math.func.Node

// -- Parsing test, check if lex and parse subsystems works fine ---------------
object ParsingTest {

  def parse(filename:String): Node = {
    val source = Source.fromFile(filename);
    val lines = source.mkString;
    source.close();
    
    try {
      var lex:Lexer = new Lexer;
      var parser:Parser = new Parser;
      
      val tokens = lex.lex(lines.toList);
      println("Lex: " + tokens);
      
      val parseTree = parser.parse(tokens);
      println("Parsed " + parseTree);
      
      parseTree;
      
    } catch {
      case e:Exception => {
        e.printStackTrace;
        null;
      } 
    }
  }
  
  def main(args:Array[String]) {
    parse("examples/sum.f");
  }

}