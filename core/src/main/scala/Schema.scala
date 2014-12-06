package uk.org.openeyes.jsonschema.core

import java.net.URI
import org.json4s.JsonAST._

object JsonRef {
  def unapply(jv: JValue): Option[URI] = jv \ "$ref" match {
      case JString(ref) => Some(new URI(ref))
      case _ => None
    }
}

object DraftV4Schema {
  def parentSchemaRefs(jv: JValue): Seq[URI] = jv \ "allOf" match {
    case JArray(jvs) => for (JsonRef(uri) <- jvs) yield uri
    case _ => Seq()
  }

  def allowedTypes(jv: JValue): Set[Type] = {
    jv \ "type" match {
      case JArray(values) => (for (JString(s) <- values) yield Type(s)).toSet
      case JString(str) => Set(Type(str))
      case _ => Set(TArray, TBoolean, TNumber, TNull, TObject, TString)
    }
  }

  def maxItems(jv: JValue): Option[BigInt] = jv \ "maxItems" match {
    case JInt(i) => Some(i)
    case _ => None
  }

  def minItems(jv: JValue): BigInt = jv \ "minItems" match {
    case JInt(i) => i
    case _ => 0
  }

  def properties(jv: JValue): Map[String, JValue] = jv \ "properties" match {
    case JObject(props) => props.toMap
    case _ => Map()
  }
}

sealed abstract class Type
case object TArray extends Type { override def toString = "array" }
case object TBoolean extends Type { override def toString = "boolean" }
case object TInteger extends Type { override def toString = "integer" }
case object TNumber extends Type { override def toString = "number" }
case object TNull extends Type { override def toString = "null" }
case object TObject extends Type { override def toString = "object" }
case object TString extends Type { override def toString = "string" }

object Type {
  def apply(str: String) = str match {
    case "array" => TArray
    case "boolean" => TBoolean
    case "integer" => TInteger
    case "number" => TNumber
    case "null" => TNull
    case "object" => TObject
    case "string" => TString
    case _ => throw new Exception("Unrecognised type: '" + str + "'")
  }
}
