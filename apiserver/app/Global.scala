import play.api._

import views._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.info("Elm API Server has started.")
    Documentation.init()
  }

  override def onStop(app: Application) {
    Logger.info("Elm API Server is shutting down...")
  }

}
