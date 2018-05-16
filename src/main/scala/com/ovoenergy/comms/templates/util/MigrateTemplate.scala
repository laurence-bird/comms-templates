package com.ovoenergy.comms.templates.util

import cats.data.Validated
import com.amazonaws.auth.DefaultAWSCredentialsProviderChain
import com.amazonaws.regions.DefaultAwsRegionProviderChain
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{AmazonS3Exception, CopyObjectResult, ListObjectsV2Request}
import com.ovoenergy.comms.model.{Channel, CommManifest, TemplateManifest}
import com.ovoenergy.comms.templates.{TemplatesContext, TemplatesRepo}

import scala.collection.JavaConverters._
import com.ovoenergy.comms.templates.s3.{AmazonS3ClientWrapper, S3Client}

class MigrateTemplate(client: AmazonS3Client, bucketName: String) {

  val c = client
  val s3ClientWrapper = new AmazonS3ClientWrapper(client, bucketName)

  private def s3File(filename: String, channel: Channel, commManifest: CommManifest): Option[String] =
    s3ClientWrapper.getUTF8TextFileContent(templateFileKey(channel, commManifest, filename))

  private def templatePrefix(channel: Channel, commManifest: CommManifest): String =
    s"${commManifest.commType.toString.toLowerCase}/${commManifest.name}/${commManifest.version}/${channel.toString.toLowerCase}"

  private def templateFileKey(channel: Channel, commManifest: CommManifest, filename: String): String =
    s"${templatePrefix(channel, commManifest)}/$filename"

  def getOldKeys() = {
    val request = new ListObjectsV2Request().withBucketName(bucketName)
    client.listObjectsV2(request).getObjectSummaries.asScala.map(_.getKey)
  }

  val templatesContext = TemplatesContext.cachingContext(new DefaultAWSCredentialsProviderChain, "dev-ovo-comms-templates")


  def updateKey(key: String): String = {
    val commNameRegex = """(\w*)""".r


    val arr = key.split("/").toList
    val arrNoCommType = arr.drop(1)
    val x = {
      val h = arrNoCommType.head
      Hash(h)
    }

    val newArr = x :: arrNoCommType.drop(1)

    newArr.mkString("/")
  }

  case class KeyResult(oldKeys: Seq[String], newKeys: Seq[String], fragments: Seq[String])

  def buildNewKeys(): KeyResult = {
    val oldKeys = s3ClientWrapper.listFiles("service")
    val (oldFragments, oldNonFragments) = oldKeys.partition(_.contains("fragments"))

    val newKeys = oldNonFragments.map(updateKey)
    KeyResult(oldNonFragments, newKeys, oldFragments)
  }

  def buildNewFragmentKey(oldKey: String) = {
    oldKey
      .split("/")
      .toList
      .drop(1)
      .mkString("/")
  }

  def doIt() = {
    val keysResult = buildNewKeys()
    val zippedKeys = keysResult.oldKeys.zip(keysResult.newKeys)

//    zippedKeys.foreach { (keys) =>
//      val (oldKey, newKey) = (keys._1, keys._2)
//      println(s"Copying from $oldKey to $newKey in $bucketName")
//      val res: CopyObjectResult = client.copyObject(bucketName, oldKey, bucketName, newKey)
//      res.getVersionId
//    }

    keysResult.fragments.foreach{ fr: String =>
      val newFR = buildNewFragmentKey(fr)
      println(s"moving fragment: $fr to $newFR")
      client.copyObject(bucketName, fr, bucketName, newFR)
    }

    println(s"Old keys found: ${keysResult.oldKeys.length}")
    println(s"New keys made: ${keysResult.newKeys.length}")
    keysResult
  }

  def removeKey(key: String) = {
    println(s"Removing $key")
    c.deleteObject(bucketName, key)
  }

  def list: Seq[String] = {
    try {
      val request = new ListObjectsV2Request().withBucketName(bucketName)
      client.listObjectsV2(request).getObjectSummaries.asScala.map(_.getKey)
    } catch {
      case e: AmazonS3Exception =>
        print("NOOO!!!!")
        Nil
    }
  }

  def removeOld() = {
    val oldKeys = s3ClientWrapper.listFiles("service")
    oldKeys.filterNot(_ == "service")
  }

  def fetchTemplate(commManifest: CommManifest, templateManifest: TemplateManifest)= {
    val t1 = TemplatesRepo.getTemplate(templatesContext, commManifest)
    val t2 = TemplatesRepo.getTemplate(templatesContext, templateManifest)
    (t1, t2)
  }
}
