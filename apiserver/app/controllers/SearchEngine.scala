package controllers

import play.api._
import play.api.libs.json._
import play.api.mvc._

import views._

import scala.collection.immutable._

object SearchEngine extends Controller {

  def reload = Action {
    Documentation.init()
    Ok("Reloaded.")
  }

  // See http://empirewindrush.com/tech/2013/12/17/cors-and-play.html
  def preflight(all: String) = Action {
    Ok("").withHeaders("Access-Control-Allow-Origin" -> "*",
      "Allow" -> "*",
      "Access-Control-Allow-Methods" -> "POST, GET",
      "Access-Control-Allow-Headers" ->
        "Origin, X-Requested-With, Content-Type, Accept, Referer, User-Agent");
  }

  def listPackages = Action {
    val jsPackages = Documentation.packages.keys
      .map(JsString)
      .toSeq

    // TODO: How should the results be sorted?
    // Idea : Github stars
    Ok(Json.stringify(JsArray(jsPackages)))
  }

  // 'package' is unfortunately a reserved keyword...
  def listVersions(elmPackage: String) = Action {
    Documentation.packages.get(elmPackage)
      .map { versions =>
        val jsVersions = versions.keys.map(JsString).toSeq
        Ok(Json.stringify(JsArray(jsVersions)))
      }
      .getOrElse(NotFound)
  }

  def listModules(elmPackage: String, version: String) = Action {
    Documentation.packages.get(elmPackage)
      .flatMap { versions => versions.get(version) }
      .map { modules =>
        val jsModules = modules.keys.map(JsString).toSeq
        Ok(Json.stringify(JsArray(jsModules)))
      }
      .getOrElse(NotFound)
  }

  def getModule(elmPackage: String, version: String,
                module_name: String) = Action {
    Documentation.packages.get(elmPackage)
      .flatMap { versions => versions.get(version) }
      .flatMap { modules => modules.get(module_name) }
      .map { module =>
        Ok(Json.stringify(module.json))
      }
      .getOrElse(NotFound)
  }

  def searchLibraries(elmPackage : String, query: String,
                      version: Option[String],
                      module_name: Option[String]) = Action {
    def jsonBoldRanges(name: String): JsArray = {
      JsArray(subsequence(query)(name)
                .map { range => JsArray(range.map { n => JsNumber(n) }) }
      )
    }

    Documentation.packages.get(elmPackage)
      .flatMap { versions => version match {
          case Some(v) => versions.get(v)
          case None => Some(versions.last._2)
      }}
      .flatMap { modules => module_name match {
          case Some(name) => modules.get(name) .map { List(_) }
          case None => Some(modules.values)
      }}
      .map { modules =>
        val results = modules
          .map { module =>
            val matches = module.values
              .map { _.name}
              .filter (isSubsequence(query))
            (module.name, matches)
          }
          .filter { case (moduleName, matches) =>
            isSubsequence(query)(moduleName) || matches.length > 0
          }
          .map { case (moduleName, matches) =>
            Json.obj(
              "module_name" -> moduleName,
              "matches" -> JsArray(matches.map { name =>
                Json.obj(
                  "name" -> name,
                  "boldRanges" -> jsonBoldRanges(name)
                  )
                }),
              "boldRanges" -> jsonBoldRanges(moduleName)
              )
          }
          .toSeq
        Ok(Json.stringify(JsArray(results)))
      }
      .getOrElse(NotFound)
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

  def subsequence(query: String) (sequence: String): List[Seq[Int]] = {
    def matchingChars(query: List[Char], sequence: List[Char]): List[Boolean] = {
      (query, sequence) match {
        case (Nil, _) => List.fill(sequence.length)(false)
        case (_, Nil) => Nil
        case (x::xs, y::ys) if x == y => true :: matchingChars(xs, ys)
        case (xs, y::ys) => false :: matchingChars(xs, ys)
      }
    }

    def trueRanges(ranges: List[(Int, Int)], elem: (Boolean, Int)): List[(Int, Int)] = elem match {
      case (value, index) =>
        (ranges, value) match {
          case ((start, end) :: rst, true) if end == index - 1 =>
            // Extend range
            (start, index) :: rst
          case (_, true) =>
            // New range
            (index, index) :: ranges
          case _ => ranges
        }
    }

    matchingChars(query.toList, sequence.toList)
      .zipWithIndex
      .foldLeft(List.empty[(Int, Int)])(trueRanges)
      .reverse
      .map { case (start, end) => Seq(start, end) }
  }
}
