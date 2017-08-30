package com.ovoenergy.comms.templates.model.template.processed.print

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.instances.option._
import cats.syntax.cartesian._
import cats.syntax.traverse._
import cats.{Applicative, Id}
import com.ovoenergy.comms.templates.ErrorsOr

import scala.language.higherKinds
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}

case class PrintTemplate[M[_]: Applicative](
    body: M[HandlebarsTemplate],
    header: Option[M[HandlebarsTemplate]],
    footer: Option[M[HandlebarsTemplate]]
) {
  def aggregate: M[PrintTemplate[Id]] = {
    (body |@| header.sequenceU |@| footer.sequenceU) map {
      PrintTemplate[Id](_, _, _)
    }
  }

  def requiredData: M[ErrorsOr[RequiredTemplateData.obj]] = {
    import cats.instances.list._
    (body |@| header.sequenceU |@| footer.sequenceU) map {
      case (s, h, t) =>
        val templates: List[HandlebarsTemplate]           = List(Some(s), h, t).flatten
        val requiredDatas: List[RequiredTemplateData.obj] = templates.map(_.requiredData)
        RequiredTemplateData.combine(requiredDatas)
    }
  }
}
