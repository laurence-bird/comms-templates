package com.ovoenergy.comms.templates.parsing.handlebars

import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import cats.syntax.either._
import com.github.jknack.handlebars.Handlebars
import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.retriever.PartialsRetriever
import org.parboiled2._
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class HandlebarsParsing(partialsRetriever: PartialsRetriever,
                        keysToNotValidate: Set[Validators.ProvidedDataKey] = Set.empty)
    extends Parsing[HandlebarsTemplate] {
  private val partialsRegex = "\\{\\{> *([a-zA-Z._]+) *\\}\\}".r

  override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
    eitherToErrorsOr {
      for {
        contentIncludingPartials <- resolvePartials(templateFile).right
        _                        <- checkTemplateCompiles(contentIncludingPartials).right
      } yield {
        buildAndValidateRequiredData(contentIncludingPartials, templateFile)
          .map(data => HandlebarsTemplate(contentIncludingPartials, data))
      }
    }
  }

  /*
      In most contexts we don't want to include data fields provided in Triggered events in the required template data
      objects, ignoreProvidedDataFields toggles this behaviour

   */
  private def buildAndValidateRequiredData(contentIncludingPartials: String,
                                           templateFile: TemplateFile): ErrorsOr[RequiredTemplateData.obj] = {
    buildRequiredTemplateData(contentIncludingPartials) andThen { data =>
      Validators.processProvidedDataFields(data, templateFile, keysToNotValidate)
    }
  }

  /**
    * Parse a template file's content as Handlebars to discover
    * what type of data would be needed to correctly populate it.
    *
    * @param rawExpandedContent The raw content of the Handlebars template,
    *                           with all partials recursively resolved and expanded.
    */
  private[handlebars] def buildRequiredTemplateData(rawExpandedContent: String): ErrorsOr[RequiredTemplateData.obj] = {
    val parser = new HandlebarsASTParser(rawExpandedContent)

    parser.WholeTemplate.run() match {
      case Success(asts) =>
        ASTProcessing.buildRequiredTemplateData(asts)
      case Failure(parseError: ParseError) =>
        Invalid(NonEmptyList.of(parseError.format(parser)))
      case Failure(other) =>
        Invalid(NonEmptyList.of(other.toString))
    }
  }

  private[handlebars] def resolvePartials(templateFile: TemplateFile): Either[String, String] = {
    val MaxRecursionDepth = 10

    def retrieveAndReplacePartial(templateContent: String,
                                  templateFile: TemplateFile,
                                  depth: Int): Either[String, String] = {
      if (depth >= MaxRecursionDepth) {
        Left("Encountered excessive recursion when expanding partials")
      } else {
        partialsRegex.findFirstMatchIn(templateContent).map(_.group(1)) match {
          case Some(partialName) =>
            // We recurse once to expand any partials referenced within the partial,
            // then recurse again to process the rest of the original input
            for {
              partialContent         <- partialsRetriever.getSharedPartial(templateFile, partialName)
              expandedPartialContent <- retrieveAndReplacePartial(partialContent, templateFile, depth + 1)
              processedContent = partialsRegex.replaceFirstIn(templateContent, expandedPartialContent)
              processRestOfInput <- retrieveAndReplacePartial(processedContent, templateFile, depth)
            } yield processRestOfInput
          case None => Right(templateContent)
        }
      }
    }

    retrieveAndReplacePartial(templateFile.content, templateFile, 0)
  }

  private[handlebars] def checkTemplateCompiles(input: String): Either[String, Unit] = {
    try {
      new Handlebars().compileInline(input)
      Right(())
    } catch {
      case NonFatal(e) => Left(s"Error compiling template: ${e.getMessage}")
    }
  }

  private def eitherToErrorsOr[A](either: Either[String, ErrorsOr[A]]): ErrorsOr[A] = {
    either match {
      case Right(result) => result
      case Left(error)   => Invalid(NonEmptyList.of(error))
    }
  }
}
