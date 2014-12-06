package uk.org.openeyes.jsonschema.doc

import java.net.URI
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import uk.org.openeyes.jsonschema.core._
import uk.org.openeyes.jsonschema.core.DraftV4Schema._

import GraphViz._

object GenClassDiagram {
  def main(args: Array[String]) = {
    if (args.length != 2) {
      throw new IllegalArgumentException("Usage: " + getClass() + " <schema dir> <output filename>")
    }

    val (schemaDir, outputFile) = (args(0), args(1))

    val schemas = new SchemaLoader(JsonMethods.parse).loadFromDir(schemaDir)

    val (nodes, inheritEdges) = schemas.map {
      case (name, schema) => (Node(name, "label" -> genLabel(name, schema)), parentSchemaRefs(schema).map(uri => Edge(basename(uri) -> name)))
    }.toSeq.unzip

    val graph = Graph()(NodeSet("shape" -> "record")(nodes))(EdgeSet("dir" -> "back", "arrowtail" -> "empty")(inheritEdges.flatten))

    val w = new java.io.FileWriter(outputFile)
    graph.format(w)
    w.close
  }

  private def genLabel(name: String, schema: JValue) = {
    "{" + name + "|" + properties(schema).foldRight(""){
      case ((name, schema), acc) => name + ": " + genTypesString(schema) +  "\\l" + acc
    } + "}"
  }

  private def genTypesString(schema: JValue) = schema match {
    case JsonRef(uri) => basename(uri)
    case schema => allowedTypes(schema).map(genTypeString(schema, _)).mkString("\\|")
  }

  private def genTypeString(schema: JValue, t: Type): String = {
    if (t == TArray) {
      (schema \ "items") match {
        case itemSchema: JObject => genTypesString(itemSchema) + " [" + minItems(schema) + ".." + maxItems(schema).getOrElse("*") + "]"
        case JArray(itemSchemas) => "(" + itemSchemas.map(genTypesString(_)).mkString(", ") + ")"
        case _ => t.toString
      }
    } else {
      t.toString
    }
  }

  private def basename(uri: URI) = uri.toString.split("[#/]").last
}
