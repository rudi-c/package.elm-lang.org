import play.api._
import play.api.mvc._

import views._

import scala.concurrent._
import ExecutionContext.Implicits.global

// See http://empirewindrush.com/tech/2013/12/17/cors-and-play.html
object CorsFilter extends Filter {
  def apply(next: (RequestHeader) => Future[Result])(rh: RequestHeader) = {

    def cors(result: Result): Result = {
      result.withHeaders( "Access-Control-Allow-Origin" -> "*",
        "Access-Control-Allow-Methods" -> "POST, GET",
        "Access-Control-Allow-Headers" ->
            "x-requested-with,content-type,Cache-Control,Pragma,Date"
      )
    }

    next(rh).map(cors)
  }
}

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.info("Elm API Server has started.")
    Documentation.init()
  }

  override def onStop(app: Application) {
    Logger.info("Elm API Server is shutting down...")
  }

  override def doFilter(action: EssentialAction) = CorsFilter(action)
}
