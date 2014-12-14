package just4fun.android.core.app

import just4fun.android.core.persist.IntVar
import project.config.logging.Logger._
import android.os.Process

trait AppController extends ActivityControllerCallbacks with ServiceControllerCallbacks {
	app: App =>
	private var activityMgr: ActivityController = _
	private var serviceMgr: ServiceController = _


	def inStandBy: Boolean = serviceMgr.inStandBy
	def isFailed: Boolean = serviceMgr.isFailed
	def isUiVisible: Boolean = activityMgr.isUiVisible
	def isUiAvailable: Boolean = activityMgr.isUiAvailable
	def lastCrashed = CrashMarker.lastCrashed

	def findService[S <: AppService](id: String = null)(implicit cxt: AppServiceContainer = null, mft: Manifest[S]): Option[S] = {
		val _cxt = if (cxt == null) serviceMgr.activeContainer else cxt
		if (_cxt != null) _cxt.findService[S](id) else None
	}
	def withService[S <: AppService](id: String = null)(f: S => Unit)(implicit cxt: AppServiceContainer = null, mft: Manifest[S]): Unit = {
		findService[S](id)(cxt, mft).foreach(s => f(s))
	}


	private[app] def initialize(): Unit = {
		activityMgr = new ActivityController(this)
		serviceMgr = new ServiceController(this)
		app.registerActivityLifecycleCallbacks(activityMgr)
		CrashMarker.init()
	}
	def start(): Unit = {
		serviceMgr.start()
		SurvivalAgent.checkForeground()
	}
	def finish(): Unit = {
		serviceMgr.finish()
		activityMgr.finish()
		SurvivalAgent.start()
		CrashMarker.markFinish()
	}
	private[app] def onFinished(): Unit = if (activityMgr.isUiAvailable) start()
	else {
		SurvivalAgent.finish()
		app.onUtilize()
		CrashMarker.clear()
		if (app.killProcessAfterExit) Process.killProcess(Process.myPid())
	}

	/* ActivityControllerCallbacks */
	override private[app] def onUiStart(): Unit = start()
	override private[app] def onUiFinish(): Unit = {
		if (!app.liveWithoutUi || isFailed) finish()
		SurvivalAgent.checkForeground()
	}
	override private[app] def onUiVisible(visible: Boolean): Unit = {
		if (serviceMgr.isServing) serviceMgr.onUiVisible(visible)
		SurvivalAgent.checkForeground()
	}

	/* ServiceControllerCallbacks */
	override private[app] def onServicesFinished(): Unit = onFinished()







	/* CRASH VAR */
	/** Indicates if and when app was crashed or killed by system */
	object CrashMarker extends Enumeration {
		val NO_CRASH, CRASH_ON_FINISH, CRASH_ON_ACTIVE = Value
		val marker = "_crashMarker_"
		var crashFlag: Int = NO_CRASH.id

		def init() = {
			val crashMarker = IntVar(marker)
			crashFlag = crashMarker.get()
			if (crashFlag != CRASH_ON_ACTIVE.id) crashMarker ~ CRASH_ON_ACTIVE.id
		}
		def markFinish() = IntVar(marker) ~ CRASH_ON_FINISH.id
		def clear() = {
			crashFlag = NO_CRASH.id
			IntVar(marker) ~ NO_CRASH.id
		}
		def wasCrashedOnFinish = crashFlag == CRASH_ON_FINISH.id
		def wasCrashedOnActive = crashFlag == CRASH_ON_ACTIVE.id
		def lastCrashed = crashFlag != NO_CRASH.id
	}
}





/* CALLBACKS */

private[app] trait ActivityControllerCallbacks {
	private[app] def onUiStart(): Unit
	private[app] def onUiFinish(): Unit
	private[app] def onUiVisible(visible: Boolean): Unit
}

private[app] trait ServiceControllerCallbacks {
	private[app] def onServicesFinished(): Unit
}

private[app] trait ServiceContainerCallbacks {
	def onFinished(container: AppServiceContainer): Unit
}
