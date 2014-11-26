package uk.org.openeyes.jsonschema.core

import java.net.URI
import org.json4s.JsonAST._

object Transform {
  def transformRefs(value: JValue, transform: (URI => URI)): JValue = value match {
    case JObject(fields) => JObject(
      fields.map(_ match {
        case ("$ref", JString(uri)) => "$ref" -> JString(transform(new URI(uri)).toString)
        case (k, v) => k -> transformRefs(v, transform)
      })
    )
    case JArray(values) => JArray(values.map(transformRefs(_, transform)))
    case v => v
  }
}
