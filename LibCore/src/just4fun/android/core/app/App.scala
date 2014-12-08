package just4fun.android.core.app

import android.app.Application
import android.content.pm.PackageManager
import just4fun.android.core.app
import just4fun.android.core.persist.IntVar
import project.config.logging.Logger._
import android.content.Context


/* GLOBAL ACCESSOR */

object App extends AppSingleton[App]



abstract class AppSingleton[T <: App] {
	// WARN: throws error if T is not an  Application subclass declared in Manifest.xml.
	private lazy val app: T = just4fun.android.core.app.appInstance.asInstanceOf[T]
	def apply(): T = app
}




/* APP class */

abstract class App extends Application with AppConfig with Loggable /*TODO with Inet with Db etc*/ {

	just4fun.android.core.app.appInstance = this
	// set thread pool [[async.NewThreadContext]]
	just4fun.android.core.async.threadPoolExecutor = threadPoolExecutor

	private var activityMgr: ActivityManager = _
	private var serviceMgr: ServiceManager = _
	private var wasKilled = false


	/* INTERNALS */

	override protected def onCreate(): Unit = {
		activityMgr = new ActivityManager
		serviceMgr = new ServiceManager
		serviceMgr.init(this, activityMgr)
		activityMgr.init(serviceMgr)
		registerActivityLifecycleCallbacks(activityMgr)
		// check if last session was killed. and reset mark
		val killedMark = IntVar("_killedMark")
		wasKilled = killedMark.get() == 1
		if (!wasKilled) killedMark ~ 1
		logw("", s"${" " * (60 - 3) }   >   APP  CREATED  #${hashCode() };  WAS KILLED ? $wasKilled")
	}
	private[app] def finalise() = {
		onExited()
		wasKilled = false
		IntVar("_killedMark") ~ 0
	}




	/* PUBLIC API */

	def start() = serviceMgr.start()
	def isActive = serviceMgr.active
	def hasUI: Boolean = activityMgr.hasUI
	def isUiVsible: Boolean = activityMgr.isVisible
	def exit() = activityMgr.exit()
	def isExited = serviceMgr.isExited

	def findService[S <: AppService](id: String)(implicit cxt: AppServiceContainer = null): Option[S] = {
		val _cxt = if (cxt == null) ServiceManager.activeContainer else cxt
		if (_cxt != null) cxt.findService(id) else None
	}
	def withService[S <: AppService](id: String)(f: S => Unit)(implicit cxt: AppServiceContainer = null): Unit = {
		findService[S](id)(cxt).foreach(s => f(s))
	}

	def wasLastSessionKilled = wasKilled


	/* UTILS */

	def hasPermission(prm: String): Boolean = getPackageManager.checkPermission(prm, getPackageName) == PackageManager.PERMISSION_GRANTED

}
