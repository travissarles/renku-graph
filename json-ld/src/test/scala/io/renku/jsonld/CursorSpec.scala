package io.renku.jsonld

import cats.implicits._
import io.circe.DecodingFailure
import io.renku.jsonld.generators.Generators.Implicits._
import io.renku.jsonld.generators.JsonLDGenerators._
import io.renku.jsonld.syntax._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CursorSpec extends WordSpec with ScalaCheckPropertyChecks {

  "as" should {

    "return success if cursor can be decoded to the requested type" in {
      forAll { json: JsonLD =>
        json.cursor.as[JsonLD] shouldBe json.asRight[DecodingFailure]
      }
    }
  }

  "downType" should {

    "return a Cursor pointing to object of the given type" in {
      forAll { (id: EntityId, entityType: EntityType, schema: Schema, property: (Property, JsonLD)) =>
        val searchedType = (schema / "type").asEntityType
        val cursor = JsonLD
          .entity(id, EntityTypes.of(entityType, searchedType), property)
          .cursor

        cursor.downType(searchedType) shouldBe cursor
      }
    }

    "return an empty Cursor if the object is not of the searched type" in {
      forAll { (id: EntityId, entityTypes: EntityTypes, schema: Schema, property: (Property, JsonLD)) =>
        JsonLD
          .entity(id, entityTypes, property)
          .cursor
          .downType((schema / "type").asEntityType) shouldBe Cursor.Empty
      }
    }
  }

  "downField" should {

    "return a Cursor pointing to a searched property" in {
      forAll { (id: EntityId, entityTypes: EntityTypes, property1: (Property, JsonLD), property2: (Property, JsonLD)) =>
        val (prop1Name, prop1Value) = property1
        val cursor = JsonLD
          .entity(id, entityTypes, property1, property2)
          .cursor
          .downField(prop1Name)

        cursor.as[JsonLD] shouldBe prop1Value.asRight[DecodingFailure]
      }
    }
  }

  "delete" should {

    "allow to remove a selected field" in {
      forAll { (id: EntityId, entityTypes: EntityTypes, property1: (Property, JsonLD), property2: (Property, JsonLD)) =>
        JsonLD
          .entity(id, entityTypes, property1, property2)
          .cursor
          .downField(property1._1)
          .delete
          .top shouldBe Some(JsonLD.entity(id, entityTypes, property2))
      }
    }

    "allow to remove a selected entity" in {
      forAll { (id: EntityId, entityTypes: EntityTypes, property: (Property, JsonLD)) =>
        JsonLD
          .entity(id, entityTypes, property)
          .cursor
          .downType(entityTypes.toList.head)
          .delete
          .top shouldBe None
      }
    }
  }
}
