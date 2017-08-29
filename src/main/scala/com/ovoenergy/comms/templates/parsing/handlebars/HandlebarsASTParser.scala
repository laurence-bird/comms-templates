package com.ovoenergy.comms.templates.parsing.handlebars

import org.parboiled2._

private[parsing] class HandlebarsASTParser(val input: ParserInput) extends Parser {

  private def WS: Rule0 = rule { zeroOrMore(" ") }

  private def RawText: Rule0 = rule { zeroOrMore((!str("{{")) ~ ANY) }

  /*
  An {{else}} looks very similar to a Ref (e.g. {{foo}}.
  We parse refs by first accepting anything that looks like a reasonable ref,
  then filtering out "else" as a special case.
   */
  private def RefText: Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum | '_' | '.')) ~> ((value: String) => test(value != "else") ~ push(value))
  }

  private def Ref: Rule1[HandlebarsAST] = rule { RefText ~> ((text: String) => HandlebarsAST.Ref(text)) }

  private def SimpleDoubleCurlies: Rule1[HandlebarsAST] = rule {
    "{{" ~ WS ~ Ref ~ WS ~ "}}"
  }

  private def SimpleTripleCurlies: Rule1[HandlebarsAST] = rule {
    "{{{" ~ WS ~ Ref ~ WS ~ "}}}"
  }

  private def Else: Rule0 = rule { "{{" ~ WS ~ "else" ~ WS ~ "}}" }

  private def StartEach: Rule1[String] = rule { "{{" ~ WS ~ "#each " ~ RefText ~ WS ~ "}}" }
  private def EndEach: Rule0           = rule { "{{" ~ WS ~ "/each" ~ WS ~ "}}" }
  private def Each: Rule1[HandlebarsAST] = rule {
    (StartEach ~ Template ~ optional(Else ~ Template) ~ EndEach) ~>
      ((refText: String,
        children: Seq[HandlebarsAST],
        elseChildren: Option[Seq[HandlebarsAST]]) =>
         HandlebarsAST.Each(refText, children, elseChildren.getOrElse(Nil)))
  }

  private def StartIf: Rule1[String] = rule { "{{" ~ WS ~ "#if " ~ RefText ~ WS ~ "}}" }
  private def EndIf: Rule0           = rule { "{{" ~ WS ~ "/if" ~ WS ~ "}}" }
  private def If: Rule1[HandlebarsAST] = rule {
    (StartIf ~ Template ~ optional(Else ~ Template) ~ EndIf) ~>
      ((refText: String,
        children: Seq[HandlebarsAST],
        elseChildren: Option[Seq[HandlebarsAST]]) => HandlebarsAST.If(refText, children, elseChildren.getOrElse(Nil)))
  }

  private def Block: Rule1[HandlebarsAST] = rule {
    Each | If | SimpleDoubleCurlies | SimpleTripleCurlies
  }

  // <html>{{#if a}}<p>{{c}}</p>{{/if}} <p>{{b}}</p></html>
  private def Template: Rule1[Seq[HandlebarsAST]] = rule { RawText ~ zeroOrMore(Block).separatedBy(RawText) ~ RawText }

  /**
    * Parse the entire input as a Handlebars template to extract the AST.
    */
  def WholeTemplate: Rule1[Seq[HandlebarsAST]] = rule { Template ~ EOI }

}
