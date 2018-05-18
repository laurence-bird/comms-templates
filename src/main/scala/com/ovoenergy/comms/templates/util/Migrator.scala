package com.ovoenergy.comms.templates.util

import java.io.{BufferedWriter, File, FileWriter}

import com.amazonaws.auth.DefaultAWSCredentialsProviderChain
import com.amazonaws.services.s3.AmazonS3Client
import com.ovoenergy.comms.templates.{TemplatesContext, TemplatesRepo}
import com.ovoenergy.comms.templates.extensions.CommManifestExtensions
import com.ovoenergy.comms.templates.s3.AmazonS3ClientWrapper

object MigrationRunner extends App {
  val client = new AmazonS3Client(new DefaultAWSCredentialsProviderChain)

  val bucketNames = List(
    "ovo-comms-template-assets",
    "ovo-comms-templates-raw",
    "ovo-comms-templates"
  )

  val migrateBucket = (bucketName: String) => new Migrator(client, bucketName).migrate()

  bucketNames.foreach(migrateBucket)
}

class Migrator(client: AmazonS3Client, bucketName: String) extends PrdTemplates with CommManifestExtensions {

  val file = new File(s"migrationLog-$bucketName.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  def log(txt: String): Unit = {
    println(txt)
    bw.write(txt + "\n")
    bw.flush()
  }

  log(s"Started to migrate $bucketName")

  val s3Client = new AmazonS3ClientWrapper(client, bucketName)
  val commTypeFolders = List("service", "marketing", "regulatory")

  def migrate() = {
    val n = commTypeFolders
      .flatMap(s3Client.listFiles)
      .map(copyFile)
      .reduce(_ + _)

    log(s"Migrated $n files in bucket $bucketName!")
    test()
  }

  def copyFile(oldFile: String) = {

    val splitFile =
      oldFile
        .split("/")
        .toList
        .drop(1)

    if(splitFile.size > 3) {
      val newFile =
        if(splitFile.head == "fragments")
          splitFile.mkString("/")
        else
          (Hash(splitFile.head) :: splitFile.drop(1)).mkString("/")

      client.copyObject(bucketName, oldFile, bucketName, newFile)
      log(s"$bucketName, $oldFile, $bucketName, $newFile")
    }

    1
  }

  val templatesContext = TemplatesContext.cachingContext(new DefaultAWSCredentialsProviderChain, bucketName)

  def test() = {
    templates.foreach{ commMan =>
      log(s"Testing for ${commMan}")
      val tempMan = commMan.toTemplateManifest

      val commManResult = TemplatesRepo.getTemplate(templatesContext, commMan)
      val tempManResult = TemplatesRepo.getTemplate(templatesContext, tempMan)

      if(commManResult.isInvalid)

      if(commManResult.isInvalid) {
        log(s"Invalid commResult! $commManResult")
      }
      if(tempManResult.isInvalid) {
        log(s"Invalid tempResult! $tempManResult")
      }
      else if(commManResult == tempManResult)
        log("Passed")
      else
        log(s"Faild ${commManResult} is not equal to ${tempManResult}")

      log("....................................")
    }
  }



}
