package uk.org.openeyes.jsonschema.core

import java.net.URI
import org.json4s._

object Serialisation {
  lazy val Serialisers = Seq(
    SchemaSerialiser,
    DraftV4Serialiser,
    AdditionalItemsSerialiser,
    ItemsSerialiser,
    AdditionalPropertiesSerialiser,
    DependencySerialiser,
    TypesSerialiser,
    TypeSerialiser,
    URISerialiser
  )

  implicit lazy val Formats = new DefaultFormats {
    override val wantsBigDecimal = true
    override val strict = true
  } ++ Serialisers
}

object SchemaSerialiser extends CustomSerializer[Schema](implicit formats => (
  { case json: JObject => json.extract[DraftV4Schema] },
  PartialFunction.empty
))

object DraftV4Serialiser extends CustomSerializer[DraftV4Schema](implicit formats => (
  {
    case json: JObject => {
      DraftV4Schema(
        json.extract[CoreAttributes],
        json.extract[NumericValidation],
        json.extract[StringValidation],
        json.extract[ArrayValidation],
        json.extract[ObjectValidation],
        json.extract[GeneralValidation],
        json.extract[Metadata]
      )
    }
  },
  {
    case schema: DraftV4Schema => {
      Extraction.decompose(schema.coreAttributes) merge
      Extraction.decompose(schema.numericValidation) merge
      Extraction.decompose(schema.stringValidation) merge
      Extraction.decompose(schema.arrayValidation) merge
      Extraction.decompose(schema.objectValidation) merge
      Extraction.decompose(schema.generalValidation) merge
      Extraction.decompose(schema.metadata)
    }
  }
))

object AdditionalItemsSerialiser extends CustomSerializer[AdditionalItems](implicit formats => (
  {
    case JBool(bool) => AdditionalItemsBoolean(bool)
    case json: JObject => AdditionalItemsSchema(json.extract[Schema])
  },
  {
    case AdditionalItemsBoolean(bool) => JBool(bool)
    case AdditionalItemsSchema(schema) => Extraction.decompose(schema)
  }
))

object ItemsSerialiser extends CustomSerializer[Items](implicit formats => (
  {
    case json: JObject => ItemsSchema(json.extract[Schema])
    case JArray(items) => ItemsSchemaList((items.map(_.extract[Schema]).toList))
  },
  {
    case ItemsSchema(schema) => Extraction.decompose(schema)
    case ItemsSchemaList(items) => JArray(items.map(Extraction.decompose).toList)
  }
))

object AdditionalPropertiesSerialiser extends CustomSerializer[AdditionalProperties](implicit formats => (
  {
    case JBool(bool) => AdditionalPropertiesBoolean(bool)
    case json: JObject => AdditionalPropertiesSchema(json.extract[Schema])
  },
  {
    case AdditionalPropertiesBoolean(bool) => JBool(bool)
    case AdditionalPropertiesSchema(schema) => Extraction.decompose(schema)
  }
))

object DependencySerialiser extends CustomSerializer[Dependency](implicit formats => (
  {
    case json: JObject => SchemaDependency(json.extract[Schema])
    case JArray(items: List[_]) => {  // work around type erasure and check that the properties are unique while we're at it
      val properties = (items map (item => {
        if (!item.isInstanceOf[JString]) throw new MappingException("Property dependencies must be strings")
        item.asInstanceOf[JString].values
      })).toSet

      if (properties.size != items.size) throw new MappingException("Property dependencies must be unique")

      PropertyDependency(properties)
    }
  },
  {
    case SchemaDependency(schema) => Extraction.decompose(schema)
    case PropertyDependency(properties) => JArray(properties.map(JString).toList)
  }
))

object TypesSerialiser extends CustomSerializer[Types](implicit formats => (
  {
    case JArray(types) => Types(types.map(_.extract[Type]).toSet)
    case singleType: JString => Types(Set(singleType.extract[Type]))
  },
  {
    case Types(types) if types.size != 1 => JArray(types.toList.map(Extraction.decompose))
    case Types(types) if types.size == 1 => Extraction.decompose(types.head)
  }
))

object TypeSerialiser extends CustomSerializer[Type](formats => (
  {
    case JString("array") => TArray
    case JString("boolean") => TBoolean
    case JString("integer") => TInteger
    case JString("number") => TNumber
    case JString("null") => TNull
    case JString("object") => TObject
    case JString("string") => TString
  },
  {
    case TArray => JString("array")
    case TBoolean => JString("boolean")
    case TInteger => JString("integer")
    case TNumber => JString("number")
    case TNull => JString("null")
    case TObject => JString("object")
    case TString => JString("string")
  }
))

object URISerialiser extends CustomSerializer[URI](formats => (
  { case JString(str) => new URI(str) },
  { case uri: URI => new JString(uri.toString) }
))
