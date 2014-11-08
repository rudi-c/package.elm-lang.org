package controllers

import play.api._
import play.api.libs.json._
import play.api.mvc._

import views._

object SearchEngine extends Controller {

  def reload = Action {
    Documentation.initFromFile()
    Ok("Reloaded.")
  }

  def listLibraries = Action {
    val libs = Documentation.libraries.keys
    .map(JsString)
    .toSeq

    // TODO: Should the results be sorted?
    Ok(Json.stringify(JsArray(libs)))
  }

  def getLibraryByName(name : String) = Action {
    val json = Documentation.libraries.get(name) match {
      case Some(library) => library.json.toString
      case None => "Error no library found."
    }

    Ok(json)
  }

  def searchLibraries(query : String) = Action {
    val results = Documentation.libraries.values
    .map { library =>
      val matches = library.values
        .map { _.name }
        .filter (isSubsequence(query))
      (library.name, matches)
    } .filter { case (libraryName, matches) =>
      isSubsequence(query)(libraryName) || matches.length > 0
    } .map { case (libraryName, matches) =>
      JsObject(Seq(
        ("library_name", JsString(libraryName)),
        ("matches", JsArray(matches.map { m => JsString(m) }))
        ))
    } .toSeq

    Ok(Json.stringify(JsArray(results)))
  }

  def isSubsequence(query: String) (sequence: String): Boolean = {
    def helper(query: List[Char], sequence: List[Char]): Boolean = {
      (query, sequence) match {
        case (Nil, _) => true
        case (_, Nil) => false
        case (x::xs, y::ys) if x == y => helper(xs, ys)
        case (xs, y::ys) => helper(xs, ys)
      }
    }
    helper(query.toList, sequence.toList)
  }
}
