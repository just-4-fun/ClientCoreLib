package just4fun.android.core.app

import just4fun.android.core.utils.TryNLog
import project.config.logging.Logger._

/* Singleton */

private[app] object ServiceManager {
	var serviceMgr: ServiceManager = _
	var activeContainer: AppServiceContainer = _
}

/* ServiceManager */

class ServiceManager extends Loggable {
	import just4fun.android.core.app.ServiceManager._
	serviceMgr = this
	var app: App = _
	var activityMgr: ActivityManager = _
	lazy protected val containers = collection.mutable.Set[AppServiceContainer]()
	var uiVisible = false

	def apply(app: App, aManager: ActivityManager) = {
		this.app = app;
		activityMgr = aManager
	}
	def active = activeContainer != null
	
	/* LIFE CYCLE */
	
	def onStart(): Unit = if (!active) {
		uiVisible = activityMgr.isVisible
		KeepAliveService.initialize(app)
		activeContainer = new AppServiceContainer(this)
		logw("", s"${containerName() }   >   APP  START")
		containers.add(activeContainer)
		app.onRegisterServices(activeContainer)
		activeContainer.start()
	}
	def onVisibilityChange(): Unit = {
		val changed = uiVisible != activityMgr.isVisible
		if (changed) uiVisible = !uiVisible
		if (changed && active) {
			logw("APP", s"${containerName() }   >   APP  VISIBLE= $uiVisible")
			activeContainer.onVisible(uiVisible)
		}
	}
	def onStop(force: Boolean = false): Unit =
		if (active && !activeContainer.stopping && (force || activeContainer.isFailed || !app.liveAfterStop)) {
			logw("APP", s"${containerName() }   >   APP  STOP")
			activeContainer.stop()
			if (!app.liveAfterStop && !app.singleInstance) activeContainer = null
		}
	def onFinalized(implicit container: AppServiceContainer): Unit = {
		containers.remove(container)
		if (container == activeContainer) activeContainer = null
		logw("APP", s"${containerName(container) }   >  APP  FINALIZED           containers.size = ${containers.size };  active ? $active")
		if (containers.isEmpty) {
			if (Dependencies.nonEmpty) loge(msg = s"Dependencies must be empty. Actually contains > ${Dependencies.mkString(", ") }")
			Dependencies.clear()
			if (activityMgr.isExited()) {
				app.exited()
				KeepAliveService.finalise()
			}
			else onStart()
			logw("APP", s"${" " * 20 }   >   APP  READY TO DIE")
		}
		System.gc()
	}
	def isServiceStartFatalError(service: AppService, err: Throwable): Boolean = {
		TryNLog { app.isServiceStartFatalError(service, err) }.getOrElse(true)
		// TODO tell UI
	}

	private def containerName(cxt: AppServiceContainer = activeContainer) = s"${" " * (50 - cxt.name.length) }[CXT: ${cxt.name }]"
}







/* SERVICE DEPENDENCIES */

/** @note Shared (singleton) service's parent should be another shared service.
       Because in case of parallel container launch their life cycles should be in sync. */
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
