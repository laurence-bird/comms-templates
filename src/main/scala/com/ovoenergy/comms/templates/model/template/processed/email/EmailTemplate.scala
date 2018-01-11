package com.ovoenergy.comms.templates.model.template.processed.email

import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.{Applicative, Apply, Id}
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
    (subject, htmlBody, textBody.sequence, sender.sequence).mapN {
      EmailTemplate[Id](_, _, _, _)
    }
  }

  def requiredData: M[ErrorsOr[RequiredTemplateData.obj]] = {
    import cats.instances.list._
    (subject, htmlBody, textBody.sequence).mapN {
      case (s, h, t) =>
        val templates: List[HandlebarsTemplate]           = List(Some(s), Some(h), t).flatten
        val requiredDatas: List[RequiredTemplateData.obj] = templates.map(_.requiredData)
        RequiredTemplateData.combine(requiredDatas)
    }
  }
}
