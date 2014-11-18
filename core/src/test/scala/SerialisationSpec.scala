package uk.org.openeyes.jsonschema.core

import org.json4s.native.Serialization
import org.scalatest.{FlatSpec,Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import Serialisation.Formats
import SchemaGen.arbSchema

class SerialisationSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Serialisation" should "be symmetrical" in forAll {(schema: Schema) => {
    Serialization.read[Schema](Serialization.write(schema)) shouldBe schema
  }}
}
