package uk.org.openeyes.jsonschema.core

import java.net.URI
import org.json4s._

object Serialisation {
  lazy val Serialisers = Seq(
    DraftV4Serialiser,
    AdditionalItemsSerialiser,
    ItemsSerialiser,
    AdditionalPropertiesSerialiser,
    DependencySerialiser,
    TypesSerialiser,
    TypeSerialiser,
    URISerialiser,
    OptionJValueSerialiser
  )

  lazy val BaseFormats = new DefaultFormats {
    override val wantsBigDecimal = true
    override val strict = true
  } ++ Serialisers + GeneralValidationSerialiser + MediaSerialiser + HyperSchemaAttributesSerialiser

  implicit lazy val Formats = BaseFormats + new SchemaSerialiser(classOf[DraftV4Schema])
}

class SchemaSerialiser[T <: Schema : Manifest](c: Class[T]) extends CustomSerializer[Schema](implicit formats => (
  {
    case json: JObject => {
      if (json.values.contains("$ref")) json.extract[SchemaRef] else json.extract[T]
    }
  },
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
        json.extract[Metadata],
        json.extract[HyperSchemaAttributes]
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
      Extraction.decompose(schema.metadata) merge
      Extraction.decompose(schema.hyperSchemaAttributes)
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
    case t: Type => JString(t.toString)
  }
))

object URISerialiser extends CustomSerializer[URI](formats => (
  { case JString(str) => new URI(str) },
  { case uri: URI => new JString(uri.toString) }
))

object OptionJValueSerialiser extends Serializer[Option[JValue]] {
  val Class = scala.reflect.classTag[Option[JValue]].runtimeClass

  def deserialize(implicit format: Formats) = {
    case (TypeInfo(Class, Some(pt)), jv) if {
      val ata = pt.getActualTypeArguments
      ata.length == 1 && ata(0) == classOf[JValue]
    } => jv match {
      case JNothing => None
      case jv => Some(jv)
    }
  }

  def serialize(implicit format: Formats) = {
    case Some(jv: JValue) => jv
    case None => JNothing
  }
}

object GeneralValidationSerialiser extends FieldSerializer[GeneralValidation](
  FieldSerializer.renameTo("types", "type"),
  FieldSerializer.renameFrom("type", "types")
)

object MediaSerialiser extends FieldSerializer[Media](
  FieldSerializer.renameTo("mediaType", "type"),
  FieldSerializer.renameFrom("type", "mediaType")
)

object HyperSchemaAttributesSerialiser extends FieldSerializer[HyperSchemaAttributes](
  serializer = { case ("links", Seq()) => None }
)
