package it.unipd.math.func

// -- A token is an elementary lexical data structure. -------------------------
abstract class Token

// -- A function token. --------------------------------------------------------
case class Func(name:Char) extends Token

// -- An operator token --------------------------------------------------------
case class Operator(symbol:Char) extends Token

// -- A number token -----------------------------------------------------------
case class Number(i:Integer) extends Token

