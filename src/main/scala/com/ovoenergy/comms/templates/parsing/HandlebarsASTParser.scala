package com.ovoenergy.comms.templates.parsing

import org.parboiled2._

private[parsing] class HandlebarsASTParser(val input: ParserInput) extends Parser {

  private def WS: Rule0 = rule { zeroOrMore(" ") }

  private def RawText: Rule0 = rule { zeroOrMore((! str("{{")) ~ ANY) }

  private def RefText: Rule0 = rule { oneOrMore(CharPredicate.AlphaNum | '_' | '.') }

  private def Ref: Rule1[HandlebarsAST] = rule { capture(RefText) ~> ((text: String) => HandlebarsAST.Ref(text)) }

  private def SimpleDoubleCurlies: Rule1[HandlebarsAST] = rule {
    "{{" ~ WS ~ Ref ~ WS ~ "}}"
  }

  private def SimpleTripleCurlies: Rule1[HandlebarsAST] = rule {
    "{{{" ~ WS ~ Ref ~ WS ~ "}}}"
  }

  // TODO optional else block
  private def StartEach: Rule1[String] = rule { "{{" ~ WS ~ "#each " ~ capture(RefText) ~ WS ~ "}}" }
  private def EndEach: Rule0 = rule { "{{" ~ WS ~ "/each" ~ WS ~ "}}" }
  private def Each: Rule1[HandlebarsAST] = rule {
    (StartEach ~ Template ~ EndEach) ~> ((refText: String, children: Seq[HandlebarsAST]) => HandlebarsAST.Each(refText, children))
  }

  // TODO optional else block
  private def StartIf: Rule1[String] = rule { "{{" ~ WS ~ "#if " ~ capture(RefText) ~ WS ~ "}}" }
  private def EndIf: Rule0 = rule { "{{" ~ WS ~ "/if" ~ WS ~ "}}" }
  private def If: Rule1[HandlebarsAST] = rule {
    (StartIf ~ Template ~ EndIf) ~> ((refText: String, children: Seq[HandlebarsAST]) => HandlebarsAST.If(refText, children))
  }

  private def Block: Rule1[HandlebarsAST] = rule {
    Each | If | SimpleDoubleCurlies | SimpleTripleCurlies
  }

  private def Template: Rule1[Seq[HandlebarsAST]] = rule { RawText ~ zeroOrMore(Block).separatedBy(RawText) ~ RawText }

  /**
    * Parse the entire input as a Handlebars template to extract the AST.
    */
  def WholeTemplate: Rule1[Seq[HandlebarsAST]] = rule { Template ~ EOI }

}

