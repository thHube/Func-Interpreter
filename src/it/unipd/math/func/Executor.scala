package it.unipd.math.func

// -- This class execute functions with their parameters. Function are        -- 
// -- evaluated using a bottom-up strategy                                    --
class Executor {

  // -- As usual, debug printing -----------------------------------------------
  private def error(msg:String) {
    System.err.println("[Executor] >> " + msg);
  }
  
  /**
   * Runs a function given in form of parse tree. At this moments there should
   * not be any exception since all checks have been done right. 
   * @param func Function to calculate on params
   * @param params Formal parameters of the function.
   * @return The result of the function. 
   */
  def execute(func:Node, params:List[Int]):Int = func match {
    // -- Zero function return 0 easy ------------------------------------------
    case Zero => 0;
    
    // -- Succ return next number ----------------------------------------------
    case Succ => params.head + 1;
    
    // -- Projection checks that number of param is right and project ----------
    case Proj(n, i) => {
      if (params.size == n) {
        params(i - 1);
      } else {
        error("Mismatch in projection, expected " + n + " params, found " + params.length);
        throw new Exception;
      }
    }
    
    // -- Function composition -------------------------------------------------
    case Cons(func, list) => compose(func, list, params);
    
    // -- Recursion call -------------------------------------------------------
    case Rec(base, induct) => recCall(base, induct, params); 
    
    // -- Minimization, this call may not end for partial functions ------------
    case Min(body) => minimize(body, params);
  }
  
  // -- A recursive call ------------------------------------------------------ 
  private def recCall(base:Node, induct:Node, params:List[Int]):Int = {
    
    if (params.head == 0) {
      // -- Base case ----------------------------------------------------------
      execute(base, params.tail)
    } else {
      // -- Recursive call -----------------------------------------------------
      val newParams = (params.head - 1)::params.tail;
      var rec:Int = execute(Rec(base, induct), newParams);
      // -- call g(x, y1, ..., yn, f(x, y1...yn)) ------------------------------
      execute(induct, newParams ::: rec::Nil);
    }
  } 
  
  // -- Minimization is [mu y. g(x1 ... xn, y) = 0] ----------------------------
  private def minimize(body:Node, params:List[Int]):Int = {
    var searchIndex:Int = 0;
    while (execute(body, params ::: searchIndex::Nil) != 0) {
      searchIndex += 1;
    }
    searchIndex;
  }
  
  // -- Compose function with the list. ----------------------------------------
  private def compose(func:Node, list:List[Node], params:List[Int]):Int = {
    // -- We need to store results ---------------------------------------------
    var results:Array[Int] = new Array(list.size);
    var auxList:List[Node] = list;
    
    // -- Loop through all function call
    for (i <- 0 until results.length) {
      results(i) = execute(auxList.head, params);
      auxList = auxList.tail;
    }
    // -- Finally call func with all his params --------------------------------
    execute(func, results.toList);
  }
  
}

