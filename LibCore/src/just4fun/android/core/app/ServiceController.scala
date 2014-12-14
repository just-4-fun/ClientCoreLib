package just4fun.android.core.app

import just4fun.android.core.utils.TryNLog
import project.config.logging.Logger._


class ServiceController(controller: ServiceControllerCallbacks) extends Loggable with ServiceContainerCallbacks {
	var activeContainer: AppServiceContainer = _
	lazy protected val containers = collection.mutable.Set[AppServiceContainer]()
	private var counter: Int = 0
	
	def isServing: Boolean = activeContainer != null
	def isFailed: Boolean = activeContainer != null && activeContainer.isFailed
	def inStandBy: Boolean = containers.isEmpty
	
	def start(): Unit = if (!isServing) {
		counter += 1
		activeContainer = new AppServiceContainer(this, s"$counter")
		containers.add(activeContainer)
		logw("", s"${containerName() }   >   APP  START")
		App.config.onRegisterServices(activeContainer)
		activeContainer.start()
	}
	def onUiVisible(visible: Boolean): Unit = activeContainer.onVisibilityChange(visible)
	def finish(): Unit = if (isServing) {
		logw("APP", s"${containerName() }   >   APP  STOP")
		activeContainer.finish()
		activeContainer = null
	}
	override def onFinished(container: AppServiceContainer): Unit = {
		containers.remove(container)
		logw("APP", s"${containerName(container) }   >  APP  UTILIZED           containers.size = ${containers.size };  serving ? $isServing")
		if (containers.isEmpty) {
			if (Dependencies.nonEmpty) {
				loge(msg = s"Dependencies must be empty. Actually contains > ${Dependencies.mkString(", ") }")
				Dependencies.clear()
			}
			logw("APP", s"${" " * 20 }   >   APP  READY TO DIE")
			controller.onServicesFinished()
		}
		System.gc()
	}
	def containerName(cxt: AppServiceContainer = activeContainer) = s"${" " * (50 - cxt.name.length) }[CXT: ${cxt.name }]"


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
