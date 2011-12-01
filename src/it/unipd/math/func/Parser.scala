package it.unipd.math.func

// -- Exception handling -------------------------------------------------------
class ParserException extends Exception

// -- Grammar for the language -------------------------------------------------
// -- 
// --     Prog ::= ZERO | SUCC | REC | MIN | COMP | PROJ | number 
// --     ZERO ::= 'Z'
// --     SUCC ::= 'S'  
// --     PROJ ::= 'P' '(' n ',' i ')'
// --     REC  ::= 'R' '[' Prog ',' Prog ']'
// --     MIN  ::= 'M' '[' Prog ']'
// --     COMP ::= 'C' '[' Prog ',' Prog {<','> Progt}']' 
// -----------------------------------------------------------------------------
class Parser {
  
  // -- Error printing utility method ------------------------------------------
  def error(msg:String) {
    System.err.println("[FUNC parser] >> " + msg);
  }

  // -- Parse prog production --------------------------------------------------
  def parseProg(tokens:List[Token]): (Node, List[Token]) = tokens match {
    // -- Function -------------------------------------------------------------
    case Func(name)::list => name match {
      case 'Z' => (Zero, list);
      case 'S' => (Succ, list);
      case 'R' => parseRec(tokens);
      case 'M' => parseMin(tokens);
      case 'C' => parseCons(tokens);
      case 'P' => parseProj(tokens);
    }
    // -- Other case ------------------------------------------------------------
    case Operator(a)::list => {
      error("Unexpected operator (" + a + ") found");
      throw new ParserException;
    }
    case _ => {
      error("Expected function or number!");
      throw new ParserException;
    }
  }
  
  // -- Parse rec production ---------------------------------------------------
  def parseRec(tokens:List[Token]): (Node, List[Token]) = tokens match{
    case Func('R')::list => list match {
      case Operator('[')::list2 => {
        val (base, rest) = parseProg(list2);
        rest.head match {
          // --  Comma after first param ---------------------------------------
    	  case Operator(',') => {
            val (induct, tail) = parseProg(rest.tail);
            tail.head match {
              case Operator(']') => (Rec(base, induct), tail.tail);
              case _ => {
                error("Expected ] at the end of recursion definition");
                throw new ParserException;
              }
            }
          }
          case _ => {
            error("Expected comma ']' in R[K, G]");
            throw new ParserException;
          }
        }
      }
      case _ => {
        error("Expected [ operator");
        throw new ParserException;
      }
    }
    case _=> {
      error("Expected R");
      throw new ParserException;
    }
  }
  
  // -- Parse min production ---------------------------------------------------
  def parseMin(tokens:List[Token]): (Node, List[Token]) = tokens match {
    case Func('M')::list => list match {
      case Operator('[')::list2 => {
        val (body, rest) = parseProg(list2);
        rest.head match {
          case Operator(']') => {
            (Min(body), rest.tail);
          }
          case _ => {
            error("Expected ] at the end of M definition")
            throw new ParserException;
          }
        }
      }
      case _ => {
        error("Expected [ parethesis before min body");
        throw new ParserException;
      }
    }
    
    case _ => {
      error("Error, misplaced minimization");
      throw new ParserException;
    }
  }
  
  // -- Parse Comp production --------------------------------------------------
  def parseCons(tokens:List[Token]):(Node, List[Token]) = tokens match {
    case Func('C')::list => list match {
      case Operator('[')::list2 => {
        // -- Composition is C[F, G_1, G_2, G_3, ....], scan F first -----------
        var (first:Node, rest:List[Token]) = parseProg(list2);
        var done:Boolean = false;
        var funcList:List[Node] = Nil;
        // -- Scan for the rest of the composition -----------------------------
        while (!done) {
          rest match {
            // -- We need to parse more functions. -----------------------------
          	case Operator(',')::list2 => {
              var (func:Node, rest2:List[Token]) = parseProg(list2);
              funcList ++= func::Nil;
              rest = rest2;
            } 
          	// -- At the end of the composition
          	case Operator(']')::list2 => {
          	  done = true;
          	  rest = list2;
          	}
          	case _ => {
          	  error("Misplaced char in composition declaration")
          	  throw new ParserException;
          	}
          }
        }
        // -- Return value calculated. -----------------------------------------
        (Cons(first, funcList), rest);
      }
      // -- Error handling -----------------------------------------------------
      case _ => {
        error("Expected parenthesis after composition");
        throw new ParserException;
      }
    }
    
    // -- Error handling, should never happens --------------------------------- 
    case _ => {
      error("Misplaced char, expected C");
      throw new ParserException;
    }
  }
 
  // -- Parse project production ----------------------------------------------- 
  def parseProj(tokens:List[Token]):(Node, List[Token]) = tokens match {
    case Func('P')::list => list match {
      // -- Must be P(n, i) any other case is an error -------------------------
      case Operator('(')::Number(n)::Operator(',')::Number(i)::Operator(')')::list2 => {
        (Proj(n, i), list2);
      }
      // -- Error -------------------------------------------------------------- 
      case _ => {
        error("Expected P before projection body")
        throw new ParserException;
      }
    }
    // -- Should not arrive here, never. ---------------------------------------
    case _ => {
      error("Misplaced projection. Something went wrong");
      throw new ParserException;
    }
  }
  
  /**
   * Parse a list of tokens. Return the parse tree if there are no syntatical 
   * errors. Throws ParserException else. 
   * @param tokens List of tokens that arrives from lex phase
   * @return Parse tree obtained
   */
  def parse(tokens:List[Token]): Node = {
    val (parsedTree, rest) = parseProg(tokens);
    if (rest != Nil) {
      error("Something is left behind ==> \n" + rest);
    }
    parsedTree;
  }
  
}