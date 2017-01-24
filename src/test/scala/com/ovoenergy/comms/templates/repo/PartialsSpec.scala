package com.ovoenergy.comms.templates.repo

import com.ovoenergy.comms.model.{Channel, CommType}
import com.ovoenergy.comms.templates.model.{FileFormat, TemplateFile}
import com.ovoenergy.comms.templates.s3.S3Client
import org.scalatest.{EitherValues, FlatSpec, Matchers}

class PartialsSpec extends FlatSpec
  with Matchers
  with EitherValues {

  def s3(contents: Map[String, String], files: Map[String, Seq[String]] = Map.empty) = new S3Client {
    override def listFiles(prefix: String): Seq[String] = files.getOrElse(prefix, Nil)
    override def getUTF8TextFileContent(key: String): Option[String] = contents.get(key)
  }

  val s3client = s3(
    contents = Map(
      "service/fragments/email/html/header.html" -> "the HTML header",
      "service/fragments/email/html/thing.html" -> "another HTML fragment",
      "service/fragments/email/txt/header.txt" -> "the text header"
    ),
    files = Map(
      "service/fragments/email/html" -> Seq(
        "service/fragments/email/html/header.html",
        "service/fragments/email/html/thing.html"
      ),
      "service/fragments/email/txt" -> Seq(
        "service/fragments/email/txt/header.txt"
      )
    )
  )

  val testObj = new Partials(s3client)

  behavior of "Partials Repo for emails"

  it should "get html partials" in {
    testObj.getSharedPartial(TemplateFile(CommType.Service, Channel.Email, FileFormat.Html, ""), "header").right.value shouldBe "the HTML header"
  }

  it should "get text partials" in {
    testObj.getSharedPartial(TemplateFile(CommType.Service, Channel.Email, FileFormat.Text, ""), "header").right.value shouldBe "the text header"
  }

  it should "fail if partial not present" in {
    testObj.getSharedPartial(TemplateFile(CommType.Service, Channel.Email, FileFormat.Text, ""), "whatevs").left.value should include("Could not find shared partial")
  }

}
