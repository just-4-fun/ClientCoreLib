package just4fun.android.core.app

import just4fun.android.core.utils.{TryNLog, BitState}
import project.config.logging.Logger
import project.config.logging.Logger._

import scala.collection.mutable
import scala.util.Try

/* Singleton */

private[app] object ServiceManager {
	var serviceMgr: ServiceManager = _
	var activeContext: AppServiceContext = _
}

/* ServiceManager */

class ServiceManager extends Loggable {
	import just4fun.android.core.app.ServiceManager._
	import just4fun.android.core.app.ServicePhase._
	serviceMgr = this
	var app: App = _
	var activityMgr: ActivityManager = _
	lazy protected val contexts = collection.mutable.Set[AppServiceContext]()
	var uiVisible = false

	def apply(app: App, aManager: ActivityManager) = {
		this.app = app;
		activityMgr = aManager
	}
	def active = activeContext != null
	
	/* LIFE CYCLE */
	
	def onStart(): Unit = if (!active) {
		uiVisible = activityMgr.isVisible
		KeepAliveService.initialize(app)
		activeContext = new AppServiceContext(this)
		logw("", s"${contextName() }   >   APP  START")
		contexts.add(activeContext)
		app.onRegisterServices(activeContext)
		activeContext.start()
	}
	def onVisibilityChange(): Unit = {
		val changed = uiVisible != activityMgr.isVisible
		if (changed) uiVisible = !uiVisible
		if (changed && active) {
			logw("APP", s"${contextName() }   >   APP  VISIBLE= $uiVisible")
			activeContext.onVisible(uiVisible)
		}
	}
	def onStop(force: Boolean = false): Unit =
		if (active && !activeContext.stopping && (force || activeContext.isFailed || !app.liveAfterStop)) {
			logw("APP", s"${contextName() }   >   APP  STOP")
			activeContext.stop()
			if (!app.liveAfterStop && !app.singleInstance) activeContext = null
		}
	def onFinalized(implicit context: AppServiceContext): Unit = {
		contexts.remove(context)
		if (context == activeContext) activeContext = null
		logw("APP", s"${contextName(context) }   >  APP  FINALIZED           contexts.size = ${contexts.size };  active ? $active")
		if (contexts.isEmpty) {
			if (Dependencies.nonEmpty) loge(msg = s"Dependencies must be empty. Actually contains > ${Dependencies.mkString(", ") }")
			Dependencies.clear()
			if (activityMgr.onExited()) KeepAliveService.finalise()
			else onStart()
			logw("APP", s"${" " * 20 }   >   APP  READY TO DIE")
		}
		System.gc()
	}
	def isServiceStartFatalError(service: AppService, err: Throwable): Boolean = {
		TryNLog { app.isServiceStartFatalError(service, err) }.getOrElse(true)
		// TODO tell UI
	}

	private def contextName(cxt: AppServiceContext = activeContext) = s"${" " * (50 - cxt.name.length) }[CXT: ${cxt.name }]"
}







/* SERVICE DEPENDENCIES */

/** @note Shared (singleton) service's parent should be another shared service.
       Because in case of parallel context launch their life cycles should be in sync. */
private[app] object Dependencies extends collection.mutable.HashSet[(AppService, AppService)] {
	def add(parent: AppService, child: AppService): Unit = if (add(parent -> child)) {
		//
		// DEFs
		def assign(p: AppService, c: AppService, recalc: Boolean = true) = {
			if (p == child) throw CyclicDependencyException(p.ID, child.ID)
			if (p.weight <= c.weight) {
				p.weight = c.weight + 1
				if (recalc) recalcParent(p)
			}
		}
		def recalcParent(_p: AppService): Unit = foreach { case (p, c) => if (c == _p) assign(p, c) }
		//
		//EXEC
		if (child.weight == 0) child.weight = 1
		assign(parent, child, parent.weight > 0)
	}
	def remove(like: (AppService, AppService) => Boolean) = retain { case (p, c) => !like(p, c) }
	
	def hasNoParent(like: AppService => Boolean)(implicit child: AppService) = !exists { case (p, c) => c == child && like(p) }
	def hasNoChild(like: AppService => Boolean)(implicit parent: AppService) = !exists { case (p, c) => p == parent && like(c) }
}
