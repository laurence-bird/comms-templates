package com.ovoenergy.comms.templates.model.template.processed.email

import cats.instances.option._
import cats.syntax.cartesian._
import cats.syntax.traverse._
import cats.{Applicative, Id}
import com.ovoenergy.comms.templates.model.{EmailSender, HandlebarsTemplate}

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

}

