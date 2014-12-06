package uk.org.openeyes.jsonschema.core

import java.net.{URI,URISyntaxException}
import org.json4s.JsonAST._
import org.json4s.JsonDSL.WithBigDecimal._
import org.scalatest.{FlatSpec,Matchers}

import DraftV4Schema._

class SchemaSpec extends FlatSpec with Matchers {
  val uris = Seq("http://example.com/example1.json", "http://example.com/example2.json")

  "JsonRef" should "match a valid JSON ref" in {
    val ref = ("$ref" -> uris(0))
    JsonRef.unapply(ref).map(_.toString) shouldBe Some(uris(0))
  }

  it should "not match something else" in {
    val jv = ("foo" -> "bar")
    JsonRef.unapply(jv) shouldBe None
  }

  it should "throw on invalid URIs" in {
    val ref = ("$ref" -> "\\")
    a [URISyntaxException] should be thrownBy JsonRef.unapply(ref)
  }

  "parentSchemaRefs" should "extract a single URI" in {
    val schema = ("allOf" -> Seq(("$ref" -> uris(0))))
    parentSchemaRefs(schema) shouldBe Seq(new URI(uris(0)))
  }

  it should "extract multiple URIs" in {
    val schema = ("allOf" -> Seq(("$ref" -> uris(0)), ("$ref" -> uris(1))))
    parentSchemaRefs(schema) shouldBe uris.map(new URI(_))
  }

  it should "extract no URIs" in {
    val schema = JObject()
    parentSchemaRefs(schema) shouldBe empty
  }

  "allowedTypes" should "match an array" in {
    val schema = ("type" -> Seq("boolean", "integer", "number"))
    allowedTypes(schema) shouldBe Set(TBoolean, TInteger, TNumber)
  }

  it should "match a single type" in {
    val schema = ("type" -> "string")
    allowedTypes(schema) shouldBe Set(TString)
  }

  it should "default to any type" in {
    val schema = JObject()
    allowedTypes(schema) shouldBe Set(TArray, TBoolean, TNumber, TNull, TObject, TString)
  }

  "maxItems" should "match an integer" in {
    val schema = ("maxItems" -> 10)
    maxItems(schema) shouldBe Some(10)
  }

  it should "default to none" in {
    val schema = JObject()
    maxItems(schema) shouldBe None
  }

  "minItems" should "match an integer" in {
    val schema = ("minItems" -> 10)
    minItems(schema) shouldBe 10
  }

  it should "default to zero" in {
    val schema = JObject()
    minItems(schema) shouldBe 0
  }

  "properties" should "get properties" in {
    val schema = ("properties" -> ("one" -> JObject()) ~ ("two" -> JObject()))
    properties(schema) shouldBe Map("one" -> JObject(), "two" -> JObject())
  }

  it should "get no properties" in {
    val schema = JObject()
    properties(schema) shouldBe empty
  }

  "Type" should "throw on unrecognised type" in {
    an [Exception] should be thrownBy Type("foo")
  }
}
