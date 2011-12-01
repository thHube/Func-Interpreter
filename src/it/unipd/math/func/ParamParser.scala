package it.unipd.math.func

// -- Parse parameters to apply to a function ----------------------------------
class ParamParser {

  // -- Print for errors -------------------------------------------------------
  def error(msg:String) {
    System.err.println("[Param parser] >> " + msg);
  }
  
  /**
   * Parse a string with parameters kinda "p1, p2, p3, ...".
   * @param tokens  
   * @return a list of int that are parsed params.
   */
  def parse(tokens:List[Token]):List[Int] = tokens match {
    // -- Parse a number -------------------------------------------------------
    case Number(i)::rest => i::parse(rest);
    
    // -- Skip commas and parethesis -------------------------------------------
    case Operator(',')::rest => parse(rest);
    case Operator('(')::rest => parse(rest);
    case Operator(')')::rest => parse(rest);
    // -- Done parsing params -------------------------------------------------- 
    case Nil => Nil;
    
    // -- Anything else is an error --------------------------------------------
    case _ => {
      error("Unexpected token(s), error");
      throw new ParserException;
    }
  }
}