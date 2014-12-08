package just4fun.android.core.app

import just4fun.android.core.app.ServicePhase._
import just4fun.android.core.utils.{TryNLog, BitState}
import project.config.logging.Logger._
import just4fun.android.core.async._
import just4fun.android.core.async.Async._
import just4fun.android.core.utils.time._

import scala.collection.mutable
import scala.util.Try


class AppServiceContainer(serviceMgr: ServiceManager) extends AsyncExecContextInitializer with Loggable {

	import ServicePhase._

	implicit val thisContainer: AppServiceContainer = this
	protected[app] var services = mutable.LinkedHashSet[AppService]().empty
	protected var timeoutMs: Long = _
	var finishing = false
	lazy private[app] val name = hashCode.toString.takeRight(3)
	var delayStartMs = deviceNow
	var delayNextMs = 0L
	protected[app] var failureOpt: Option[Throwable] = None
	def isFailed = failureOpt.nonEmpty


	/* SERVICE MANAGMENT */
	
	def findService[S <: AppService : Manifest](id: String): Option[S] = services.collectFirst {
		case s: S if s.ID == id => s
	}

	private[app] def registerService(implicit s: AppService): Unit = {
		services add s
		wakeup()
	}
	protected[app] def onServiceStartFailed(err: Throwable)(implicit s: AppService): Unit = if (!finishing) {
		if (serviceMgr.isServiceStartFatalError(s, err)) failureOpt = Some(err)
		// TODO ? stop instance and hint user
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
	}
	def finish(): Unit = {
		finishing = true
		// reorder services in order of dependencies (parents after children)
		services = mutable.LinkedHashSet[AppService]() ++ services.toSeq.sortBy(_.weight)
		services.foreach (s => if (s.container == this) s.unregister(false))
		timeoutMs = deviceNow + App().timeoutDelay
		postTik()
	}
	protected[app]def onVisibilityChange(visible: Boolean): Unit = services foreach { s =>
		if (s.phase == ACTIVE) TryNLog { s.onVisibilityChange(visible) }
	}


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
				if (s.phase == ACTIVE && !s.isFinishing) startedN += 1
			}
			if (s.phase == FINALIZED) services.remove(s)
		}
		//
		val finalized = totalN == 0
		val started = !finalized && startedN == totalN
		//
		if (finalized) finalize()
		else if (finishing && isTimeout) {changed = true; onTimeout()}
		//
		if (finalized || (started && !finishing)) sleep()
		else postTik(nextDelay)
		logv("tikState", s"stopping ? $finishing; all=${services.size};  total= $totalN;  started= $startedN;  optime= ${deviceNow - t0 }")
		//
		// DEFs
		//
		def finalize() = {
			postFinalize()
			serviceMgr.onFinalized
		}
		def isTimeout = if (timeoutMs > 0 && deviceNow > timeoutMs) {timeoutMs = 0; true } else false
		def onTimeout() = services foreach (s => if (s.container == this) s.setTimedOut())
		def nextDelay: Long = {
			val delay = if (changed) {delayStartMs = deviceNow; 50}
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
