package com.ovoenergy.comms.templates.model.template.processed

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.instances.option._
import cats.syntax.traverse._
import cats.{Applicative, Apply, Id}
import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.RequiredTemplateData
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import com.ovoenergy.comms.templates.model.template.processed.sms.SMSTemplate

import scala.language.higherKinds

/* A full template for a comm, which will include templates for one or more channels */
case class CommTemplate[M[_]: Applicative](
                               email: Option[M[EmailTemplate[Id]]],
                               sms: Option[M[SMSTemplate[Id]]]
                             ) {

  def validate: ErrorsOr[Unit] = {
    if (email.isEmpty && sms.isEmpty) Invalid(NonEmptyList.of("Template has no channels defined"))
    else Valid(())
  }

  def aggregate: M[CommTemplate[Id]] =
    Apply[M].map2(email.sequenceU, sms.sequenceU){ case (e, s) => CommTemplate[Id](e, s) }

  def requiredData: M[ErrorsOr[RequiredTemplateData.obj]] = {
    Apply[M].map2(email.sequenceU, sms.sequenceU){
      case (Some(e), Some(s)) =>
        Apply[ErrorsOr].map2(e.requiredData, s.requiredData)(List(_, _)) andThen RequiredTemplateData.combine
      case (Some(e), None) => e.requiredData
      case (None, Some(s)) => s.requiredData
      case (None, None)       => Invalid(NonEmptyList.of("No templates to combine"))
    }
  }

}

