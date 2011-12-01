package it.unipd.math.func

// -- For reporting errors -----------------------------------------------------
class ArityIncompatibilityFound extends Exception

// -- Take a parse tree and validate it analyzing function arity ---------------
class AritySolver {

  private def error(msg:String) {
    System.err.println("[Arity solver] >> " + msg);
  }
  
  /**
   * Check arity of the function. If everything goes fine return the arity of
   * function read. If something go wrong throws an exception.
   * @param tree Parse Tree.
   * @return Arity corresponding to function read.  
   */
  def check(tree:Node): Int = tree match {
    case Zero | Succ => 1;
    case Proj(n, i) => n;
    case Cons(fst, list) => checkCons(Cons(fst, list));
    case Rec(base, induc) => checkRec(Rec(base, induc));
    case Min(body) => checkMin(Min(body));
  }
  
  // -- Support func to check a whole list -------------------------------------
  private def checkList(n:Int, list:List[Node]):Boolean = list match{
    case Nil => true;
    case a::rest => if (check(a) == n) {
      checkList(n, rest);
    } else false;
  }
  
  // -- Control if cons has the right arity -----------------------------------
  private def checkCons(cons:Cons):Int = cons match {
    case Cons(fst, list) => {
      // -- We expect C^n[F^m, m*{G^n}] if arity of F is different than       --
      // -- number of Gs is an error, then we check Gs to have the same arity --
      if (check(fst) != list.size) {
        error("Expected a different number of funs in composition!");
        throw new ArityIncompatibilityFound;
      } else {
        var n = check(list.head);
        if (checkList(n, list.tail)) n;
        else {
          error("Composition param list has incompatible arity");
          throw new ArityIncompatibilityFound;
        }
      }
    }
  }
  
  // -- Check recursion --------------------------------------------------------
  private def checkRec(rec:Rec):Int = rec match {
    case Rec(base, induc) => {
      // -- We expect functions to be R^{n+1}[K^n, G^{n+2}] --------------------
      var k:Int = check(base);
      var g:Int = check(induc);
      if ((k + 2) == g ) k + 1;
      else {
        error("Mismatch in recurion: k = " + k + ", g  = " + g);
        throw new ArityIncompatibilityFound;
      }
    }
  } 
  
  // -- Check for minimization -------------------------------------------------
  private def checkMin(min:Min):Int = min match {
    case Min(body) => {
      // -- Expected is M^n[G^{n + 1}] -----------------------------------------
      var g = check(body);
      if (g <= 0) {
        error("Expected a positive g, found " + g);
        throw new ArityIncompatibilityFound;
      } else g - 1;
    }
  }        
  
}