package views

import play.api._
import play.api.libs.json._
import play.api.Play.current

import org.apache.commons.io._

import scala.collection.immutable._

class Type(json: JsValue) {
  val name = (json \ "name").asOpt[String]
  val tag = (json \ "tag").asOpt[String]
  val args = (json \ "args").asOpt[JsArray]
    .getOrElse(new JsArray())
    .value .map { new Type(_) }
  val result = (json \ "result").asOpt[JsValue].map { new Type(_) }

  // TODO: "extension", "fields"
}

class Value(json: JsValue) {
  val name = (json \ "name").as[String]
  val comment = (json \ "comment").asOpt[String]
  val associativity = (json \ "associativity").asOpt[String]
  val precendence = (json \ "precendence").asOpt[String]
  val raw = (json \ "raw").as[String]
  val valueType = new Type(json \ "raw")
}

class Datatype(json: JsValue) {
  val name = (json \ "name").as[String]
  val comment = (json \ "comment").as[String]
  val raw = (json \ "raw").as[String]
  val typeVariables = (json \ "typeVariables").as[Seq[String]]

  // TODO: "constructors"
}

class Library(json: JsValue) {
  val name = (json \ "name").as[String]
  val document = (json \ "document").as[String]
  val datatypes = (json \ "datatypes").as[JsArray].value.map { new Datatype(_) }
  val values = (json \ "values").as[JsArray].value.map { new Value(_) }

  // TODO: "aliases"
}

object Documentation {
  val docsFile = "docs/sampledocs.json"
  var libraries = TreeMap[String, Library]()

  def initFromFile() {
    Play.resourceAsStream(docsFile) foreach { inputStream =>
      val docs = IOUtils.toString(inputStream, "UTF-8");
      val json = Json.parse(docs)
      val libList = json.as[JsArray].value
        .map { new Library(_) }
        .map { library => (library.name, library) }
      libraries = TreeMap(libList : _*)
    }
  }
}
