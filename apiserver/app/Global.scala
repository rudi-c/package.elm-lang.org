import play.api._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.info("Elm API Server has started.")
    Documentation.initFromFile()
  }

  override def onStop(app: Application) {
    Logger.info("Elm API Server is shutting down...")
  }

}
