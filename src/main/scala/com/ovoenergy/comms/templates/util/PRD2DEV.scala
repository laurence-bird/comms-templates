package com.ovoenergy.comms.templates.util

import com.amazonaws.auth.DefaultAWSCredentialsProviderChain
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{AmazonS3Exception, ListObjectsV2Request}
import com.ovoenergy.comms.model.{CommManifest, TemplateManifest}
import com.ovoenergy.comms.templates.{TemplatesContext, TemplatesRepo}
import com.ovoenergy.comms.templates.s3.AmazonS3ClientWrapper

import scala.collection.JavaConverters._

object Runner extends App {
  val client = new AmazonS3Client(new DefaultAWSCredentialsProviderChain)
  val prdBucketName = "ovo-comms-templates-raw"
  val devBucketName = s"dev-$prdBucketName"

  new PRD2DEV(client, prdBucketName, devBucketName).copyPrdToDev()
}

class PRD2DEV(client: AmazonS3Client, prdBucketName: String, devBucketName: String) extends PrdTemplates {

  val prdS3Client = new AmazonS3ClientWrapper(client, prdBucketName)
  val devS3Client = new AmazonS3ClientWrapper(client, devBucketName)

  def copyPrdToDev() = {
    prdS3Client.listFiles("").foreach(key => client.copyObject(prdBucketName, key, devBucketName, key))

//    prdS3Client.listFiles("").foreach(println)
//    println("+++++++++++++++++++++++++++++++++")
//
//    def list(bucketName: String): Seq[String] = {
//      try {
//        val request = new ListObjectsV2Request().withBucketName(bucketName)
//        client.listObjectsV2(request).getObjectSummaries.asScala.map(_.getKey)
//      } catch {
//        case e: AmazonS3Exception =>
//          print("NOOO!!!!")
//          Nil
//      }
//    }
//
//    val prdTemplatesContext = TemplatesContext.cachingContext(new DefaultAWSCredentialsProviderChain, prdBucketName)
//    val devTemplatesContext = TemplatesContext.cachingContext(new DefaultAWSCredentialsProviderChain, devBucketName)
//
//    def fetchTemplate()= {
//      templates.foreach{ commMan =>
//        val t1 = TemplatesRepo.getTemplate(prdTemplatesContext, commMan)
//        val t2 = TemplatesRepo.getTemplate(devTemplatesContext, commMan)
//        println(t1 + " vs " + t2)
//      }
//    }
//
//    fetchTemplate()
//    list(devBucketName).foreach(println)
//    devS3Client.listFiles("service").foreach(println)
//    removeFile()
//
//    def removeFile(): Unit = {
//      devS3Client.listFiles("").foreach{ key =>
//
//        client.deleteObject(devBucketName, key)
//
//      }
//    }
  }

}
