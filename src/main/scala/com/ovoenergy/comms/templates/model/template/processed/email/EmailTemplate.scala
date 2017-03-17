package com.ovoenergy.comms.templates.model.template.processed.email

import cats.instances.option._
import cats.syntax.cartesian._
import cats.syntax.traverse._
import cats.{Applicative, Id}
import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.{EmailSender, HandlebarsTemplate, RequiredTemplateData}

import scala.language.higherKinds

case class EmailTemplate[M[_]: Applicative](
    subject: M[HandlebarsTemplate],
    htmlBody: M[HandlebarsTemplate],
    textBody: Option[M[HandlebarsTemplate]],
    sender: Option[M[EmailSender]]
) {

  def aggregate: M[EmailTemplate[Id]] = {
    (subject |@| htmlBody |@| textBody.sequenceU |@| sender.sequenceU) map {
      EmailTemplate[Id](_, _, _, _)
    }
  }

  def requiredData: M[ErrorsOr[RequiredTemplateData.obj]] = {
    import cats.instances.list._
    (subject |@| htmlBody |@| textBody.sequenceU) map {
      case (s, h, t) =>
        val templates: List[HandlebarsTemplate]                     = List(Some(s), Some(h), t).flatten
        val requiredDatas: ErrorsOr[List[RequiredTemplateData.obj]] = templates.map(_.requiredData).sequenceU
        requiredDatas.andThen(RequiredTemplateData.combine)
    }
  }
}
