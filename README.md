# Comms Templates Library

[![CircleCI](https://circleci.com/gh/ovotech/comms-templates.svg?style=svg)](https://circleci.com/gh/ovotech/comms-templates)

## What is a comms Template?

A template consists of all the artifacts required the build a communication, across all channels (email, sms, etc), to send to a recipient.
 
Templates are stored and versioned in S3, and are split by type (Service, Regulatory and Marketing).
 
An example template in S3 could be made up of the following files:
* Email:
  * Subject
  * Html Body
  * Text Body (optional)
  * Sender (optional)
* SMS: (yet to be implemented)
  * Body
  * Sender (optional)
 
Each of these files contains a [handlebars template](http://handlebarsjs.com/expressions.html).
 
## What does the library do 

This is a library that collects a comms Template from S3, returning the following:
* The contents of each template artifact
  * Expanding out any references to [partials](http://handlebarsjs.com/partials.html)
  * Checking that the contents form a valid handlebars template
* The data required to fulfill the template 
  * Checking there are no inconsistencies, with the template identifiers, across all the template artifacts
  * Checking that the template identifiers, referencing data provided by the comms platform, are correct

## Example

Using the provided (non-caching) TemplateContext, we can retrieve the details of the `cc-payment-taken` communication, as follows:

```scala
import cats.data.Validated.Valid
import com.amazonaws.auth.EnvironmentVariableCredentialsProvider
import com.ovoenergy.comms.model.{CommManifest, CommType}
import com.ovoenergy.comms.templates.{TemplatesContext, TemplatesRepo}

object Example extends App {

  val creds = new EnvironmentVariableCredentialsProvider()
  val context = TemplatesContext.nonCachingContext(creds)
  val manifest = CommManifest(CommType.Service, "cc-payment-taken", "0.3")

  TemplatesRepo.getTemplate(context, manifest) match {
    case Valid(template) =>
      template.combineRequiredData match {
        case Valid(data) => println(s"The required data for the template is $data")
        case invalid     => println(s"Errors found in the required data $invalid")
      }
    case invalid => println(s"The template has errors $invalid")
  }
}
```

This would yield the following output:

```
The required data for the template is obj(Map(currencySymbol -> string, amount -> string, date -> string, cardEnding -> string, transactionId -> string))
```

## Errors

The various aspects of the returned CommTemplate are wrapped by the ErrorsOr data type, which is just a type alias for the [Validated](http://eed3si9n.com/herding-cats/Validated.html) datatype provided by Cats. 

In this way the library is able to return as much information about template issues, concatenated into the ErrorsOr, rather than just failing fast

## How to use

In build.sbt:

```
resolvers += Resolver.bintrayRepo("ovotech", "maven")

libraryDependencies += "com.ovoenergy" %% "comms-templates" % "see badge above for latest version"
```

## Working on the library

### tut

The code samples in the README file are checked using [tut](https://github.com/tpolecat/tut).

This means that the `README.md` file is generated from `src/main/tut/README.md`. If you want to make any changes to the README, you should:

1. Edit `src/main/tut/README.md`
2. Run `sbt tut` to regenerate `./README.md`
3. Commit both files to git
