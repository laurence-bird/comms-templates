package com.ovoenergy.comms.templates.model

import cats.{Applicative, Id}
import cats.instances.option._
import cats.syntax.traverse._
import cats.syntax.cartesian._

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

