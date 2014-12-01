package just4fun.android.core.app

import android.app.Application
import project.config.logging.Logger._
import android.content.Context

object App {

	var app: App = _
	val activityMgr: ActivityManager = new ActivityManager
	val serviceMgr: ServiceManager = new ServiceManager

	private def init(a: App) {
		app = a
		logw("", s"${" " * (60 -3) }   >   APP  CREATED  #${app.hashCode()}")
		serviceMgr(app, activityMgr)
		activityMgr(app, serviceMgr)
		app.registerActivityLifecycleCallbacks(activityMgr)
	}

	/* PUBLIC USAGE */

	def apply(): Context = app

	def config: AppConfig = app

	def exit() = activityMgr.exit()

	def findService[S <: AppService](id: String)(implicit cxt: AppServiceContext = null): Option[S] = {
		val _cxt = if (cxt == null) ServiceManager.activeContext else cxt
		if (_cxt != null) cxt.findService(id) else None
	}
	def withService[S <: AppService](id: String)(f: S => Unit)(implicit cxt: AppServiceContext = null): Unit = {
		findService[S](id)(cxt).foreach(s => f(s))
	}
}


abstract class App extends Application with AppConfig with Loggable /*TODO with Inet with Db etc*/ {
	App.init(this)
	// TODO ? move to onCreate to avoid usage before Application instance ready
//	override def onCreate(): Unit = App(this)


	/** The place to register services */
	def onRegisterServices(implicit context: AppServiceContext): Unit
	/**
	 * @param service
	 * @return true - if error is fatal and App will be set in FAILED state; false - if App should continue
	 */
	def isServiceStartFatalError(service: AppService, err: Throwable): Boolean = {
		logv("onServiceStartFailed", s"${service.ID};  Error: ${err.getMessage}")
		service match {
			case _ => false // decide fail App or not
		}
	}
	/**

	 */
	def onExited(): Unit = ()
}
