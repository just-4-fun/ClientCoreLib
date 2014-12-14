package just4fun.android.core.app

import android.app.Activity
import android.app.Application.ActivityLifecycleCallbacks
import android.os.Bundle
import just4fun.android.core.app.ActivityState._
import scala.ref.WeakReference
import just4fun.android.core.utils._
import project.config.logging.Logger._
import just4fun.android.core.async._
import just4fun.android.core.async.Async._

class ActivityController(controller: ActivityControllerCallbacks) extends ActivityLifecycleCallbacks with Loggable {
	protected var activity: WeakRefActivity = WeakRefActivity(null)
	protected val activities = collection.mutable.WeakHashMap[Activity, Boolean]()
	protected var state: ActivityState.Value = NONE
	var reconfiguring = false
	var lastVisible = false
	val (aSTART, aVISIBLE_CHANGE, aSTOP) = (0, 1, 2)

	def isUiVisible: Boolean = state == RESUMED
	def isUiAvailable: Boolean = !(activity.isEmpty || activity().isFinishing)

	def finish(): Unit = if (isUiAvailable) {
		val a = activity()
		activities.foreach { case (_a, b) => if (_a != a && !_a.isFinishing) _a.finish() }
		a.finish()
	}

	override protected def onActivityCreated(a: Activity, savedState: Bundle): Unit = onStateChange(a, CREATED)
	override protected def onActivityStarted(a: Activity): Unit = onStateChange(a, STARTED)
	override protected def onActivityResumed(a: Activity): Unit = onStateChange(a, RESUMED)
	override protected def onActivityPaused(a: Activity): Unit = onStateChange(a, PAUSED)
	override protected def onActivityStopped(a: Activity): Unit = onStateChange(a, STOPPED)
	override protected def onActivityDestroyed(a: Activity): Unit = onStateChange(a, DESTROYED)
	override protected def onActivitySaveInstanceState(a: Activity, savedState: Bundle): Unit = ()

	protected def onStateChange(a: Activity, newStt: ActivityState.Value): Unit = {
		var action = -1
		newStt match {
			case CREATED => val pva = activity.get.orNull
				activity = WeakRefActivity(a)
				activities += (a -> true)
				if (!reconfiguring && (pva == null || pva.isFinishing)) action = aSTART
			case STARTED => activity = WeakRefActivity(a)
			case RESUMED => activity = WeakRefActivity(a)
				if (!reconfiguring) action = aVISIBLE_CHANGE
				reconfiguring = false
			case PAUSED => if (activity =? a && a.isChangingConfigurations) reconfiguring = true
			case STOPPED => if (activity =? a && !reconfiguring) action = aVISIBLE_CHANGE
			case DESTROYED => activities -= a
				if (activity =? a) {
					if (a.isFinishing) action = aSTOP
					activity = WeakRefActivity(null)
				}
			case _ => loge(msg = s"[onActivityStateChange] unknown state: ")
		}
		val isCurrent = activity.isEmpty || activity =? a
		if (isCurrent) state = newStt
		val reason = if (a.isFinishing) "finishing" else if (reconfiguring) "reconfiguring" else ""
		// exec action on serviceMgr
		action match {
			case `aVISIBLE_CHANGE` if lastVisible != isUiVisible => lastVisible = !lastVisible
				controller.onUiVisible(lastVisible)
			case `aSTART` => controller.onUiStart()
			case `aSTOP` => controller.onUiFinish()
			case _ =>
		}
		logv("onActivityStateChange", s"Activity= ${if (isCurrent) "CURR" else "       " };  state= ${newStt.toString };  reason= ${if (nonEmpty(reason)) reason }")
	}




	/* WeakRefActivity HELPER */
	case class WeakRefActivity(var a: Activity) extends WeakReference[Activity](a) {
		a = null
		def =?(v: Activity): Boolean = if (get.isEmpty) false else get.get == v
		def isEmpty = get.isEmpty
		def notEmpty = !isEmpty
	}


}