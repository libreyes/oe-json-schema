package uk.org.openeyes.jsonschema.core

import java.net.URI

class SchemaSet[T <: Schema](uriScheme: String, schemas: Map[String, T])
