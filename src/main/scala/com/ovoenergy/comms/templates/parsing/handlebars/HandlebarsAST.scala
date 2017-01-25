package com.ovoenergy.comms.templates.parsing.handlebars

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
    *
    * or
    *
    * ```
    * {{#each items}}
    *   {{this.name}}
    * {{else}}
    *   {{somethingElse}}
    * {{/each}}
    * ```
    *
    */
  case class Each(text: String, children: Seq[HandlebarsAST], elseChildren: Seq[HandlebarsAST]) extends HandlebarsAST

  /**
    * An 'if' block, e.g.
    *
    * ```
    * {{#if order}}
    *   {{order.amount}}
    * {{/each}}
    * ```
    *
    * or
    *
    * ```
    * {{#if order}}
    *   {{order.amount}}
    * {{else}}
    *   {{somethingElse}}
    * {{/each}}
    * ```
    */
  case class If(text: String, children: Seq[HandlebarsAST], elseChildren: Seq[HandlebarsAST]) extends HandlebarsAST

}
