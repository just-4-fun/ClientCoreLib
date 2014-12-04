package just4fun.android.core.app

import android.app.Application
import just4fun.android.core.persist.IntVar
import project.config.logging.Logger._
import android.content.Context

object App {

	var app: App = _
	val activityMgr: ActivityManager = new ActivityManager
	val serviceMgr: ServiceManager = new ServiceManager

	private def init(a: App) {
		app = a
		logw("", s"${" " * (60 - 3) }   >   APP  CREATED  #${app.hashCode() }")
		serviceMgr(app, activityMgr)
		activityMgr(app, serviceMgr)
		app.registerActivityLifecycleCallbacks(activityMgr)
	}

	/* PUBLIC USAGE */

	def apply(): Context = app

	def config: AppConfig = app

	def start() = serviceMgr.onStart()
	def isActive = serviceMgr.active
	def exit() = activityMgr.exit()

	def findService[S <: AppService](id: String)(implicit cxt: AppServiceContainer = null): Option[S] = {
		val _cxt = if (cxt == null) ServiceManager.activeContainer else cxt
		if (_cxt != null) cxt.findService(id) else None
	}
	def withService[S <: AppService](id: String)(f: S => Unit)(implicit cxt: AppServiceContainer = null): Unit = {
		findService[S](id)(cxt).foreach(s => f(s))
	}

	def wasKilled = app.wasKilled

}


abstract class App extends Application with AppConfig with Loggable /*TODO with Inet with Db etc*/ {

	override def onCreate(): Unit = {
		App.init(this)
		wasKilled
		logv("APP", s"WAS KILLED ? $wasKilled")
	}


	/** The place to register services */
	def onRegisterServices(implicit context: AppServiceContainer): Unit
	/**
	 * @param service
	 * @return true - if error is fatal and App will be set in FAILED state; false - if App should continue
	 */
	def isServiceStartFatalError(service: AppService, err: Throwable): Boolean = {
		logv("onServiceStartFailed", s"${service.ID };  Error: ${err.getMessage }")
		service match {
			case _ => false // decide fail App or not
		}
	}
	/**
	  */
	def onExited(): Unit = ()


	/* INTERNAL */

	private lazy val wasKilled = {
		val killedMark = IntVar("_killedMark")
		val killed = killedMark.get() == 1
		if (!killed) killedMark ~ 1
		killed
	}
	private[app] def exited() = {
		onExited()
		IntVar("_killedMark") ~ 0
	}

}
