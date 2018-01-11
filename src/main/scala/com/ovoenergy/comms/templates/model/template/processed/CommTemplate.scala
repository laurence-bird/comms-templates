package com.ovoenergy.comms.templates.model.template.processed

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.instances.option._
import cats.instances.list._
import cats.syntax.traverse._
import cats.{Applicative, Apply, Id}
import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.RequiredTemplateData
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import com.ovoenergy.comms.templates.model.template.processed.print.PrintTemplate
import com.ovoenergy.comms.templates.model.template.processed.sms.SMSTemplate

import scala.language.higherKinds

/* A full template for a comm, which will include templates for one or more channels */
case class CommTemplate[M[_]: Applicative](
    email: Option[M[EmailTemplate[Id]]],
    sms: Option[M[SMSTemplate[Id]]],
    print: Option[M[PrintTemplate[Id]]]
) {

  def checkAtLeastOneChannelDefined: ErrorsOr[Unit] = {
    if (email.isEmpty && sms.isEmpty && print.isEmpty) Invalid(NonEmptyList.of("Template has no channels defined"))
    else Valid(())
  }

  def aggregate: M[CommTemplate[Id]] =
    Apply[M].map3(email.sequence, sms.sequence, print.sequence) { case (e, s, p) => CommTemplate[Id](e, s, p) }

  def requiredData: M[ErrorsOr[RequiredTemplateData.obj]] = {
    Apply[M].map3(email.sequence, sms.sequence, print.sequence) {
      case (None, None, None) => Invalid(NonEmptyList.of("No templates to combine"))
      case (e, s, p) => {
        val requiredData: List[ErrorsOr[RequiredTemplateData.obj]] =
          List(e.map(_.requiredData), s.map(_.requiredData), p.map(_.requiredData)).flatten

        requiredData.sequence.andThen(RequiredTemplateData.combine)
      }
    }
  }

}
