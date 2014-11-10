package views

import play.api._
import play.api.libs.json._
import play.api.Play.current

import org.apache.commons.io._

import scala.collection.immutable._

class Type(val json: JsValue) {
  val name = (json \ "name").asOpt[String]
  val tag = (json \ "tag").asOpt[String]
  val args = (json \ "args").asOpt[JsArray]
    .getOrElse(new JsArray())
    .value .map { new Type(_) }
  val result = (json \ "result").asOpt[JsValue].map { new Type(_) }

  // TODO: "extension", "fields"
}

class Value(val json: JsValue) {
  val name = (json \ "name").as[String]
  val comment = (json \ "comment").asOpt[String]
  val associativity = (json \ "associativity").asOpt[String]
  val precendence = (json \ "precendence").asOpt[String]
  val raw = (json \ "raw").as[String]
  val valueType = new Type(json \ "raw")
}

class Datatype(val json: JsValue) {
  val name = (json \ "name").as[String]
  val comment = (json \ "comment").as[String]
  val raw = (json \ "raw").as[String]
  val typeVariables = (json \ "typeVariables").as[Seq[String]]

  // TODO: "constructors"
}

class Module(val json: JsValue) {
  val name = (json \ "name").as[String]
  val document = (json \ "document").as[String]
  val datatypes = (json \ "datatypes").as[JsArray].value.map { new Datatype(_) }
  val values = (json \ "values").as[JsArray].value.map { new Value(_) }

  // TODO: "aliases"
}

object Documentation {
  val docsFile = "docs/sampledocs.json"
  var packages = TreeMap[String, TreeMap[String, TreeMap[String, Module]]]()

  def init() {
    val packagesList = Play.getFile("docs/").listFiles
      .filter { _.isDirectory }
      .map { file =>
        val packageName = file.getName()
        val versionsList = file.listFiles
          .filter { _.isDirectory }
          .map { file =>
            val version = file.getName()
            val module = if (file.isDirectory && file.listFiles.size == 1 &&
                             file.listFiles.head.getName() == "docs.json") {
              Some(initFromFile("docs/" + packageName + "/" + version + "/docs.json"))
            } else {
              println("Warning : Expected a single file, docs.json. Directory ignored.")
              None
            }
            module map { (version, _) }
          }
          .flatten // get rid of options
        (packageName, TreeMap(versionsList : _*))
      }
    packages = TreeMap(packagesList : _*)
  }

  def initFromFile(docsFilePath: String): TreeMap[String, Module] = {
    val file = Play.getFile(docsFilePath)
    val lines = scala.io.Source.fromFile(file).mkString

    val json = Json.parse(lines)
    val libList = json.as[JsArray].value
      .map { new Module(_) }
      .map { library => (library.name, library) }
    TreeMap(libList : _*)
  }
}
