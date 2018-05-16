package com.ovoenergy.comms.templates.util

import com.amazonaws.auth.DefaultAWSCredentialsProviderChain
import com.amazonaws.services.s3.{AmazonS3Client, AmazonS3ClientBuilder}
import com.ovoenergy.comms.model.{CommManifest, Marketing, Service}
import com.ovoenergy.comms.templates.extensions.CommManifestExtensions
import org.scalatest.{FlatSpec, FunSuite, Matchers}

class MigrateTemplateSpec extends FlatSpec with Matchers with CommManifestExtensions{

  val client = new AmazonS3Client(new DefaultAWSCredentialsProviderChain)

  val templateMigrator = new MigrateTemplate(client, "dev-ovo-comms-templates")


//  it should "Migrate the things" in {
//    val key = "service/BoostSample/1.0/print/body.html"
//
//    val res = templateMigrator.updateKey(key)
//    println(res)
//
//    res shouldBe s"${Hash("BoostSample")}/1.0/print/body.html"
//
//    val result = templateMigrator.doIt()
//    result.newKeys.size shouldBe result.oldKeys.size
//  }

  it should "be able to retrieve from templateManifest and commManifest" in {
    uat.foreach{ commMan =>
      println(s"Testing for ${commMan}")
      val templMan = commMan.toTemplateManifest

      val (templFromCM, templFromTM)  = templateMigrator.fetchTemplate(commMan, templMan)
      if(templFromCM.isInvalid)
        println(s"Invalid noooo! $templFromCM")
      templFromCM shouldBe templFromTM
    }
  }

}
