package controllers

import play.api._
import play.api.mvc._

import views._

object SearchEngine extends Controller {

  def reload = Action {
    Documentation.initFromFile()
    Ok("Reloaded.")
  }

  def listLibraries = Action {
    Ok("Listing...")
  }

  def getLibraryByName(name : String) = Action {
    Ok("Retrieving...")
  }

  def searchLibraries(name : String) = Action {
    Ok("Searching...")
  }
}
