package com.ovoenergy.comms.templates

import cats.data.ValidatedNel
import shapeless._

package object model {

  type TemplateDatum = String :+: Seq[TemplateDatumWrapper] :+: Map[String, TemplateDatumWrapper] :+: CNil

  case class TemplateDatumWrapper(value: TemplateDatum)

  type ErrorsOr[A] = ValidatedNel[String, A]

}
