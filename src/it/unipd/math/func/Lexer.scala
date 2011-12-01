package it.unipd.math.func

// -- Exception to signal errors at lex level ----------------------------------
class NonEligibleSymbolFound extends Exception

// -- Lexical analysis module. Read the file and create a list of tokens to   --
// -- use during parsing.                                                     --
class Lexer {

  private def isOtherChar(symbol:Char):Boolean = symbol match {
    case ' ' | '\n' | '\t' | '\r' => true;
    case _ => false;
  };
  
  // -- Understand if the given symbol is a function call ----------------------
  private def isFunction(symbol:Char):Boolean = symbol match {
    case 'Z' | 'S' | 'C' | 'M' | 'R' | 'P' => true;
    case _ => false;
  }
  
  // -- Understand if the given symbol is an operator --------------------------
  private def isOperator(symbol:Char):Boolean = symbol match {
    case '[' | ']'| '(' | ')' | ',' => true;
    case _ => false;
  }
  
  // -- Understand if a symbol is a number -------------------------------------
  private def isNumber(list:Char):Boolean = list match {
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true;
    case _ => false;
  }
  
  // -- Parse a number, return the number and the list without the parsed ------
  private def parseNumber(list:List[Char]): (Integer, List[Char]) = list.head match {
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
      var (number, newList) = parseNumber(list.tail);
      // if (list.tail != Nil) {
        if (isNumber(list.tail.head)) {
          number += 10 * (list.head.toInt - '0'.toInt);
        // }
      } else {
        number += (list.head.toInt - '0'.toInt);
      }
      (number, newList);
    }
    case _ => (0, list);
  }
  
  /**
   * Analyze the input file and create the list of tokens.
   * @param symbols List of symbols that compose the input file. 
   * @return List of token created during lexical analysis
   */
  def lex(symbols:List[Char]):List[Token] = symbols match {
    case Nil => Nil;
    case _ => {
	  val head:Char = symbols.head;
	  if (isOperator(head)) {
	    Operator(head)::lex(symbols.tail)
	  } else if (isFunction(head)){
       Func(head)::lex(symbols.tail);
	  } else if (isNumber(head)) {
       val (number, rest) = parseNumber(symbols);
       Number(number)::lex(rest);
	  } else if (isOtherChar(head)){
       lex(symbols.tail);
      } else {
	    System.err.println("[FUNC lexer] >> Found " + head + " and stopped.");
	    throw new NonEligibleSymbolFound;
	  }
    }
  }
}