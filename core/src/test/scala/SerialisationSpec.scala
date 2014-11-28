package uk.org.openeyes.jsonschema.core

import org.json4s.JsonAST._
import org.json4s.native.Serialization
import org.scalatest.{FlatSpec,Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import Serialisation.Formats
import SchemaGen.arbSchema

class SerialisationSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Serialisation" should "be symmetrical" in forAll {(schema: Schema) => {
    Serialization.read[Schema](Serialization.write(schema)).toString shouldBe schema.toString
  }}

  it should "deserialise a single type without an array" in {
    Serialization.read[DraftV4Schema]("""{"type":"integer"}""").generalValidation.types shouldBe Some(Types(Set(TInteger)))
  }

  it should "serialise a single type without an array" in {
    Serialization.write(DraftV4Schema(generalValidation = GeneralValidation(types = Some(Types(Set(TInteger)))))) shouldBe """{"type":"integer"}"""
  }

  it should "handle enums properly" in {
    Serialization.read[GeneralValidation]("""{"enum":[1,2]}""") shouldBe GeneralValidation(Some(Seq(JInt(1), JInt(2))))
  }
}
