package uk.org.openeyes.jsonschema.core

import org.json4s.JsonAST._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

// Generate schemas (not necessarily valid)
object SchemaGen {
  implicit lazy val arbSchema = Arbitrary(genSchema())

  def genSchema(level: Int = 0): Gen[Schema] = {
    implicit lazy val arbSchema = Arbitrary(genSchema(level + 1))

    lazy val nonEmpty = for {
      coreAttributes <- genCoreAttributes
      numericValidation <- genNumericValidation
      stringValidation <- genStringValidation
      arrayValidation <- genArrayValidation
      objectValidation <- genObjectValidation
      generalValidation <- genGeneralValidation
      metadata <- genMetadata
      hyperSchemaAttributes <- genHyperSchemaAttributes
    } yield DraftV4Schema(
      coreAttributes,
      numericValidation,
      stringValidation,
      arrayValidation,
      objectValidation,
      generalValidation,
      metadata,
      hyperSchemaAttributes
    )

    lazy val empty = const(DraftV4Schema())

    level match {
      case 0 => nonEmpty
      case 1 => oneOf(nonEmpty, empty)
      case 2 => frequency((1, nonEmpty), (2, empty))
      case _ => empty
    }
  }

  lazy val genCoreAttributes = for {
    $schema <- unlikelyOption(genURI)
    id <- unlikelyOption(genURI)
    $ref <- unlikelyOption(genURI)
  } yield CoreAttributes($schema, id, $ref)

  lazy val genNumericValidation = for {
    multipleOf <- unlikelyOption(genNumber)
    maximum <- unlikelyOption(genNumber)
    exclusiveMaximum <- unlikelyOption(arbitrary[Boolean])
    minimum <- unlikelyOption(genNumber)
    exclusiveMinimum <- unlikelyOption(arbitrary[Boolean])
  } yield NumericValidation(multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum)

  lazy val genStringValidation = for {
    maxLength <- unlikelyOption(arbitrary[BigInt])
    minLength <- unlikelyOption(arbitrary[BigInt])
    pattern <- unlikelyOption(genRegex)
  } yield StringValidation(maxLength, minLength, pattern)

  def genArrayValidation(implicit arbSchema: Arbitrary[Schema]) = {
    for {
      additionalItems <- unlikelyOption(oneOf(resultOf(AdditionalItemsBoolean), resultOf(AdditionalItemsSchema)))
      items <- unlikelyOption(oneOf(resultOf(ItemsSchema), resize(8, resultOf(ItemsSchemaList))))
      maxItems <- unlikelyOption(arbitrary[BigInt])
      minItems <- unlikelyOption(arbitrary[BigInt])
      uniqueItems <- unlikelyOption(arbitrary[Boolean])
    } yield ArrayValidation(additionalItems, items, maxItems, minItems, uniqueItems)
  }

  def genObjectValidation(implicit arbSchema: Arbitrary[Schema]) = {
    for {
      maxProperties <- unlikelyOption(arbitrary[BigInt])
      minProperties <- unlikelyOption(arbitrary[BigInt])
      required <- unlikelyOption(resize(8, nonEmptyListOf(resize(16, alphaStr))))
      additionalProperties <- unlikelyOption(oneOf(resultOf(AdditionalPropertiesBoolean), resultOf(AdditionalPropertiesSchema)))
      properties <- unlikelyOption(resize(8, mapOf[String,Schema](zip(resize(16, alphaStr), arbitrary[Schema]))))
      patternProperties <- unlikelyOption(resize(8, mapOf(zip(genRegex, arbitrary[Schema]))))
    } yield ObjectValidation(maxProperties, minProperties, required, additionalProperties, properties, patternProperties)
  }

  def genGeneralValidation(implicit arbSchema: Arbitrary[Schema]) = {
    for {
      enum <- unlikelyOption(resize(8, nonEmptyListOf(genJValue)))
      t <- unlikelyOption(someOf(TArray, TBoolean, TInteger, TNumber, TNull, TObject, TString).map(t => Types(t.toSet)))
      allOf <- unlikelyOption(resize(4, nonEmptyListOf(arbitrary[Schema])))
      anyOf <- unlikelyOption(resize(4, nonEmptyListOf(arbitrary[Schema])))
      oneOf <- unlikelyOption(resize(4, nonEmptyListOf(arbitrary[Schema])))
      not <- unlikelyOption(arbitrary[Schema])
      definitions <- unlikelyOption(resize(4, nonEmptyMap(zip(resize(16, alphaStr), arbitrary[Schema]))))
    } yield GeneralValidation(enum, t, allOf, anyOf, oneOf, not, definitions)
  }

  lazy val genMetadata = for {
    title <- unlikelyOption(resize(16, alphaStr))
    description <- unlikelyOption(alphaStr)
    default <- unlikelyOption(genJValue)
  } yield Metadata(title, description, default)

  def genHyperSchemaAttributes(implicit arbSchema: Arbitrary[Schema]) = {
    for {
      links <- frequency(4 -> Seq(), 1 -> resize(4, listOf(genLdo)))
      fr <- unlikelyOption(resize(16, alphaStr))
      media <- unlikelyOption(for {
        be <- unlikelyOption(resize(16, alphaStr))
        mt <- unlikelyOption(resize(16, alphaStr))
      } yield Media(be, mt))
      ro <- unlikelyOption(arbitrary[Boolean])
      ps <- unlikelyOption(genURI)
    } yield HyperSchemaAttributes(links, fr, media, ro, ps)
  }

  def genLdo(implicit arbSchema: Arbitrary[Schema]) = {
    for {
      href <- resize(16, alphaStr)
      rel <- resize(16, alphaStr)
      title <- unlikelyOption(resize(16, alphaStr))
      targetSchema <- unlikelyOption(arbitrary[Schema])
      mediaType <- unlikelyOption(resize(16, alphaStr))
      method <- unlikelyOption(resize(4, alphaStr))
      schema <- unlikelyOption(arbitrary[Schema])
    } yield LinkDescriptionObject(href, rel, title, targetSchema, mediaType, method, schema)
  }

  lazy val genURI = for {
    scheme <-  resize(4, alphaStr) suchThat (_.length > 0)
    ssp <- resize(16, alphaStr) suchThat (_.length > 0)
    fragment <- resize(16, alphaStr)
  } yield new java.net.URI(scheme, ssp, fragment)

  lazy val genNumber = oneOf(
    arbitrary[BigDecimal] suchThat (n => {  // FIXME
      try {
        new java.math.BigDecimal(n.toString)
        true
      } catch {
        case e: NumberFormatException => false
      }
    }),
    for { n <- arbitrary[BigInt] } yield BigDecimal(n)
  )

  lazy val genRegex = resize(16, alphaStr)  // FIXME

  lazy val genJValue: Gen[JValueWrapper] = oneOf(const(JNull), resultOf(JBool), resultOf(JInt)).map(JValueWrapper(_))

  def unlikelyOption[T](g: Gen[T]) = {
    frequency((1, g.map(Some.apply)), (3, const(None)))
  }
}
