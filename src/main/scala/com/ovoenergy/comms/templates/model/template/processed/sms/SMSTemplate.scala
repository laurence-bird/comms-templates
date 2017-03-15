package com.ovoenergy.comms.templates.model.template.processed.sms

import cats.{Applicative, Apply, Id}
import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}

import scala.language.higherKinds

case class SMSTemplate[M[_]: Applicative](textBody: M[HandlebarsTemplate]) {

  def aggregate: M[SMSTemplate[Id]] =
    Apply[M].map(textBody)(SMSTemplate[Id])

  def requiredData: M[ErrorsOr[RequiredTemplateData.obj]] =
    Apply[M].map(textBody)(_.requiredData)

}

