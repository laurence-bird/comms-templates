package com.ovoenergy.comms

import cats.data.ValidatedNel

package object templates {

  type ErrorsOr[A] = ValidatedNel[String, A]

}
