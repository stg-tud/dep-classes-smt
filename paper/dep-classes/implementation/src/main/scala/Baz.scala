object Baz extends App {
  // https://www.geeksforgeeks.org/scala-parser-combinators/
  // https://index.scala-lang.org/scala/scala-parser-combinators/scala-parser-combinators/2.1.1

  import scala.util.parsing.combinator._

  class Arith extends JavaTokenParsers {
    def expr: Parser[Any] = term~rep("+"~term | "-"~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
  }

  object ParseExpr extends Arith {
    def foo(arg: String): ParseResult[Any] = parse(expr, arg)
    def bar(arg: String): ParseResult[Any] = parseAll(expr, arg)
  }

  println(ParseExpr.foo("1+2+3*4"))
  println(ParseExpr.bar("1+2+3*4"))
  println(ParseExpr.bar("4*(5+7)"))
}
