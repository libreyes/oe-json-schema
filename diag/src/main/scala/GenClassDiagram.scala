package uk.org.openeyes.jsonschema.diag

import org.json4s.native.Serialization
import uk.org.openeyes.jsonschema.core._
import uk.org.openeyes.jsonschema.core.Serialisation.Formats

import GraphViz._

object GenClassDiagram {
  def main(args: Array[String]) = {
    if (args.length != 3) {
      throw new IllegalArgumentException("Usage: " + getClass() + " <uri scheme> <schema dir> <output filename>")
    }

    val schemaSet = new SchemaLoader(Serialization.read[DraftV4Schema](_)).loadFromDir(args(0), args(1))

    def genLabel(name: String, schema: DraftV4Schema) = {
      "{" + name + "|" + schema.objectValidation.properties.fold("")(_.foldRight(""){
        case ((name, schema: DraftV4Schema), acc) => name + ": " + genTypesString(schema) +  "\\l" + acc
      }) + "}"
    }

    def genTypesString(schema: DraftV4Schema) = {
      schema.refURI.map(_.getSchemeSpecificPart)
        .orElse(schema.generalValidation.types.map(_.types.map(genTypeString(schema, _)).mkString("\\|")))
        .getOrElse("?")
    }

    def genTypeString(schema: DraftV4Schema, t: Type): String = {
      if (t == TArray) {
        schema.arrayValidation.items match {
          case Some(ItemsSchema(itemSchema: DraftV4Schema)) => genTypesString(itemSchema) + " [" + schema.arrayValidation.minItems.getOrElse(0) + ".." + schema.arrayValidation.maxItems.getOrElse("*") + "]"
          case Some(ItemsSchemaList(itemSchemas)) => throw new Exception("TODO")
          case _ => throw new Exception("No array validation defined for schema with type array: " + schema)
        }
      } else {
        t.toString
      }
    }

    val (nodes, inheritEdges) = schemaSet.schemas.map {
      case (name, schema) => (Node(name, "label" -> genLabel(name, schema)), schema.parentSchemaRefs.map(uri => Edge(uri.getSchemeSpecificPart -> name)))
    }.toSeq.unzip

    val graph = Graph()(NodeSet("shape" -> "record")(nodes))(EdgeSet("dir" -> "back", "arrowtail" -> "empty")(inheritEdges.flatten))

    val w = new java.io.FileWriter(args(2))
    graph.format(w)
    w.close
  }
}
