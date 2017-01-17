package com.ovoenergy.comms.templates.parsing

sealed trait HandlebarsAST

object HandlebarsAST {

  /**
    * A reference to a piece of context data,
    * e.g. `{{ foo }}` or `{{ a.b.c }}` or `{{ this.foo }}`
    *
    * @param text the text inside the curlies
    */
  case class Ref(text: String) extends HandlebarsAST

  /**
    * An 'each' block, e.g.
    *
    * ```
    * {{#each items}}
    *   {{this.name}}
    * {{/each}}
    * ```
    */
  case class Each(text: String, children: Seq[HandlebarsAST]) extends HandlebarsAST

  /**
    * An 'if' block, e.g.
    *
    * ```
    * {{#if order}}
    *   {{order.amount}}
    * {{/each}}
    * ```
    */
  case class If(text: String, children: Seq[HandlebarsAST]) extends HandlebarsAST

}
