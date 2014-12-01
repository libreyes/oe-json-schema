package uk.org.openeyes.jsonschema.doc

import java.net.URI
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization
import uk.org.openeyes.jsonschema.core._
import uk.org.openeyes.jsonschema.core.Serialisation.Formats

import GraphViz._

object GenClassDiagram {
  def main(args: Array[String]) = {
    if (args.length != 2) {
      throw new IllegalArgumentException("Usage: " + getClass() + " <schema dir> <output filename>")
    }

    val (schemaDir, outputFile) = (args(0), args(1))

    val schema = new SchemaLoader(JsonMethods.parse, _.extract[DraftV4Schema]).loadFromDir(schemaDir)

    val (nodes, inheritEdges) = schema.generalValidation.definitions.getOrElse(Seq()).map {
      case (name, schema) => (Node(name, "label" -> genLabel(name, schema)), schema.parentSchemaRefs.map(uri => Edge(basename(uri) -> name)))
    }.toSeq.unzip

    val graph = Graph()(NodeSet("shape" -> "record")(nodes))(EdgeSet("dir" -> "back", "arrowtail" -> "empty")(inheritEdges.flatten))

    val w = new java.io.FileWriter(outputFile)
    graph.format(w)
    w.close
  }

  private def genLabel(name: String, schema: Schema) = schema match {
    case schema: DraftV4Schema =>
      "{" + name + "|" + schema.objectValidation.properties.fold("")(_.foldRight(""){
        case ((name, schema), acc) => name + ": " + genTypesString(schema) +  "\\l" + acc
      }) + "}"
  }

  private def genTypesString(schema: Schema) = schema match {
    case SchemaRef(uri) => basename(uri)
    case schema: DraftV4Schema => schema.generalValidation.types.fold("?")(_.types.map(genTypeString(schema, _)).mkString("\\|"))
  }

  private def genTypeString(schema: DraftV4Schema, t: Type): String = {
    if (t == TArray) {
      schema.arrayValidation.items match {
        case Some(ItemsSchema(itemSchema)) => genTypesString(itemSchema) + " [" + schema.arrayValidation.minItems.getOrElse(0) + ".." + schema.arrayValidation.maxItems.getOrElse("*") + "]"
        case Some(ItemsSchemaList(itemSchemas)) => throw new Exception("TODO")
        case _ => throw new Exception("No array validation defined for schema with type array:\n" + Serialization.writePretty(schema))
      }
    } else {
      t.toString
    }
  }

  private def basename(uri: URI) = uri.toString.split("[#/]").last
}
