package just4fun.android.core.app

import just4fun.android.core.app.ServicePhase._
import just4fun.android.core.utils.{TryNLog, BitState}
import project.config.logging.Logger._
import just4fun.android.core.async._
import just4fun.android.core.async.Async._
import just4fun.android.core.utils.time._

import scala.collection.mutable
import scala.util.Try


class AppServiceContainer(serviceMgr: ServiceContainerCallbacks, val name: String) extends AsyncExecContextInitializer with Loggable {

	import ServicePhase._

	implicit val thisContainer: AppServiceContainer = this
	protected[app] var services = mutable.LinkedHashSet[AppService]().empty
	protected var timeoutMs: Long = _
	private  var finishing = false
	var delayStartMs = deviceNow
	var delayNextMs = 0L
	protected[app] var failureOpt: Option[Throwable] = None


	/* SERVICE MANAGMENT */
	
	def findService[S <: AppService : Manifest](id: String = null): Option[S] =
		if (id == null) services.collectFirst { case s: S => s }
		else services.collectFirst { case s: S if s.ID == id => s }
	def isServing: Boolean = !finishing && !isFailed
	def isFinishing: Boolean = finishing
	def isFailed: Boolean = failureOpt.nonEmpty


	private[app] def register(implicit s: AppService): Unit = {
		services.add(s)
		wakeup()
	}
	protected[app] def onStartFailed(err: Throwable)(implicit s: AppService): Unit = if (!finishing && s.isCrucial) {
		failureOpt = Option(err)
	}


	/* CONTEXT LIFE CYCLE */

	def start(): Unit = {
		preInitialize()
		// assign FirstPriorityFeature dependencies
		val (fps, nfps) = services.partition(_.isInstanceOf[FirstPriorityFeature])
		for (parent <- fps; child <- nfps) Dependencies.add(parent, child)
		// reorder services in order of dependencies (parents before children)
		services = mutable.LinkedHashSet[AppService]() ++ services.toSeq.sortBy(-_.weight)
		postTik()
		logi("start", "SERVICES>>\n"+services.map(_.stateInfo()).mkString("\n"))
	}
	def finish(): Unit = {
		finishing = true
		// reorder services in order of dependencies (parents after children)
		services = mutable.LinkedHashSet[AppService]() ++ services.toSeq.sortBy(_.weight)
		services.foreach(s => if (s.container == this) s.unregister(false))
		timeoutMs = deviceNow + App.config.timeoutDelay
		postTik()
	}
	protected[app] def onVisibilityChange(visible: Boolean): Unit = services.foreach {s => if (s.container == this) s.onVisibilityChange(visible) }



	/* INTERNAL API */

	protected def postTik(delayMs: Long = 0): Unit = post("TIK", delayMs) { tik() }
	protected[app] def wakeup() = if (delayNextMs - deviceNow > 100) {delayNextMs = 0; postTik() }
	protected def sleep(): Unit = {
		delayNextMs = Long.MaxValue
		asyncExecContext.clear()
	}

	protected def tik(): Unit = {
		/* TODO remove */ val t0 = deviceNow
		var totalN, startedN = 0
		var changed = false
		//
		services foreach { s =>
			if (s.container == this) {
				if (s.nextPhase()) changed = true
				totalN += 1
				if (s.isServing) startedN += 1
			}
			if (s.phase == UTILIZED || s.phase == INIT) services.remove(s)
		}
		//
		val finished = totalN == 0
		val started = !finished && startedN == totalN
		//
		if (finished) finish()
		else if (finishing && isTimeout) {changed = true; onTimeout() }
		//
		if (finished || (started && !finishing)) sleep()
		else postTik(nextDelay)
		logv("tikState", s"stopping ? $finishing; all=${services.size };  total= $totalN;  started= $startedN;  optime= ${deviceNow - t0 }")
		//
		// DEFs
		//
		def finish() = {
			postUtilize()
			serviceMgr.onFinished(this)
		}
		def isTimeout = if (timeoutMs > 0 && deviceNow > timeoutMs) {timeoutMs = 0; true } else false
		def onTimeout() = services foreach (s => if (s.container == this) s.setInterrupting())
		def nextDelay: Long = {
			val delay = if (changed) {delayStartMs = deviceNow; 50 }
			else {
				val mult = if (finishing) 2 else 1
				val time = deviceNow - delayStartMs
				if (time < 500) 100 * mult
				else if (time < 2000) 250 * mult
				else if (time < 10000) 1000 * mult
				else if (time < 60000) 4000 * mult
				else 10000
			}
			delayNextMs = deviceNow + delay
			delay
		}
	}

}
