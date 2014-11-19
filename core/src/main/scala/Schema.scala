package uk.org.openeyes.jsonschema.core

import java.lang.Class
import java.net.URI
import org.json4s.JsonAST._

trait Schema

case class DraftV4Schema(
  coreAttributes: CoreAttributes = CoreAttributes(),
  numericValidation: NumericValidation = NumericValidation(),
  stringValidation: StringValidation = StringValidation(),
  arrayValidation: ArrayValidation = ArrayValidation(),
  objectValidation: ObjectValidation = ObjectValidation(),
  generalValidation: GeneralValidation = GeneralValidation(),
  metadata: Metadata = Metadata()
) extends Schema

case class CoreAttributes(
  $schema: Option[URI] = None,
  id: Option[URI] = None,
  $ref: Option[URI] = None
)

case class NumericValidation(
  multipleOf: Option[BigDecimal] = None,
  maximum: Option[BigDecimal] = None,
  exclusiveMaximum: Option[Boolean] = None,
  minimum: Option[BigDecimal] = None,
  exclusiveMinimum: Option[Boolean] = None
)

case class StringValidation(
  maxLength: Option[BigInt] = None,
  minLength: Option[BigInt] = None,
  pattern: Option[String] = None
)

sealed abstract class AdditionalItems
case class AdditionalItemsBoolean(value: Boolean) extends AdditionalItems
case class AdditionalItemsSchema(value: Schema) extends AdditionalItems

sealed abstract class Items
case class ItemsSchema(value: Schema) extends Items
case class ItemsSchemaList(values: Seq[Schema]) extends Items

case class ArrayValidation(
  additionalItems: Option[AdditionalItems] = None,
  items: Option[Items] = None,
  maxItems: Option[BigInt] = None,
  minItems: Option[BigInt] = None,
  uniqueItems: Option[Boolean] = None
)

sealed abstract class AdditionalProperties
case class AdditionalPropertiesBoolean(value: Boolean) extends AdditionalProperties
case class AdditionalPropertiesSchema(value: Schema) extends AdditionalProperties

sealed abstract class Dependency
case class SchemaDependency(value: Schema) extends Dependency
case class PropertyDependency(properties: Set[String]) extends Dependency

case class ObjectValidation(
  maxProperties: Option[BigInt] = None,
  minProperties: Option[BigInt] = None,
  required: Option[Seq[String]] = None,
  additionalProperties: Option[AdditionalProperties] = None,
  properties: Option[Map[String,Schema]] = None,
  patternProperties: Option[Map[String,Schema]] = None,
  dependencies: Option[Map[String,Dependency]] = None
)

sealed abstract class Type
case object TArray extends Type
case object TBoolean extends Type
case object TInteger extends Type
case object TNumber extends Type
case object TNull extends Type
case object TObject extends Type
case object TString extends Type

case class Types(types: Set[Type])

case class GeneralValidation(
  enum: Option[Seq[JValueWrapper]] = None,
  types: Option[Types] = None,
  allOf: Option[Seq[Schema]] = None,
  anyOf: Option[Seq[Schema]] = None,
  oneOf: Option[Seq[Schema]] = None,
  not: Option[Schema] = None,
  definitions: Option[Map[String,Schema]] = None
)

case class Metadata(
  title: Option[String] = None,
  description: Option[String] = None,
  default: Option[JValueWrapper] = None
)

case class JValueWrapper(value: JValue)
