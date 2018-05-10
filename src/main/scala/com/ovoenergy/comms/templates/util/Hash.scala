package com.ovoenergy.comms.templates.util

import java.security.MessageDigest

object Hash {

  /*
      Used for hashing legacy Comm names to retrieve the new Template ID.

      **Note**
      lowercase only due to s3 buckets being case sensitive, and URLS not.

   */
  def apply(str: String): String = {
    val hash = MessageDigest
      .getInstance("MD5")
      .digest(str.getBytes)

    new String(hash).toLowerCase
  }
}
