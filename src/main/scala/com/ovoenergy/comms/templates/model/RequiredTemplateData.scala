package com.ovoenergy.comms.templates.model

sealed trait RequiredTemplateData

object RequiredTemplateData {
  case object string extends RequiredTemplateData
  case object optString extends RequiredTemplateData
  case object strings extends RequiredTemplateData

  case class obj(fields: Map[String, RequiredTemplateData]) extends RequiredTemplateData
  case class optObj(fields: Map[String, RequiredTemplateData]) extends RequiredTemplateData
  case class objs(fields: Map[String, RequiredTemplateData]) extends RequiredTemplateData
}

/*
hello {{ firstName }}
{{#each items}}
  Item: {{this}}
{{/each}}

Map(
  "firstName" -> string,
  "items" -> strings
)

case class TemplateData(firstName: String, items: Seq[String])
 */

/*
hello {{ user.firstName }}
{{#each items}}
  Item: {{this.name}} {{this.amount}}
{{/each}}

After parsing:

Seq(
  Ref("user.firstName"),
  Each("items", Seq(
    Ref("this.name"),
    Ref("this.amount")
  ))
)

Map(
  "user" -> obj(Map("firstName" -> string)),
  "items" -> objs(Map("name" -> string, "amount" -> string))
)

case class User(firstName: String)
case class Item(name: String, amount: String)
case class TemplateData(user: User, items: Seq[Item])
 */

/*
hello {{ firstName }}
{{#if gasReading}}
  Gas reading: {{gasReading}}
{{/if}}

Map(
  "firstName" -> string,
  "gasReading" -> optString
)
 */

/*
hello {{ firstName }}
{{#if gasReading}}
  Gas: {{gasReading.date}} {{gasReading.amount}}
{{/if}}

Map(
  "firstName" -> string,
  "gasReading" -> optObj(Map("date" -> string, "amount" -> string))
)
 */
