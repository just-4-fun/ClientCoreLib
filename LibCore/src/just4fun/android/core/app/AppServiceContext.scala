package just4fun.android.core.app

import just4fun.android.core.app.ServiceState._
import just4fun.android.core.utils.{TryNLog, BitState}
import project.config.logging.Logger._
import just4fun.android.core.async._
import just4fun.android.core.async.Async._
import just4fun.android.core.utils.time._

import scala.collection.mutable
import scala.util.Try


class AppServiceContext(serviceMgr: ServiceManager) extends AsyncExecContextInitializer with Loggable {

	import ServiceState._

	implicit val context: AppServiceContext = this
	protected[app] var services = mutable.LinkedHashSet[AppService]().empty
	protected var timeoutMs: Long = _
	var stopping = false
	protected[app] var failureOpt: Option[Throwable] = None
	def isFailed = failureOpt.nonEmpty
	lazy private[app] val name = context.hashCode.toString.takeRight(3)
	var delayCounter = 0
	var delayStartMs = deviceNow
	var delayNextMs = 0L


	/* SERVICE MANAGMENT */
	
	def findService[S <: AppService : Manifest](id: String): Option[S] = services.collectFirst {
		case s: S if s.ID == id => s
	}

	private[app] def registerService(s: AppService): Unit = {
		services add s
		wakeup()
	}
	private[app] def unregisterService(s: AppService): Unit = if (services remove s) s.onUnregistered
	protected[app] def onServiceStartFailed(service: AppService, err: Throwable): Unit = {
		// TODO ? stop instance or hint user
		if (serviceMgr.isServiceStartFatalError(service, err)) failureOpt = Some(err)
	}


	/* CONTEXT LIFE CYCLE */

	def start(): Unit = {
		preInitialize()
		// reorder services in order of dependencies (parents before children)
		services = mutable.LinkedHashSet[AppService]() ++ services.toSeq.sortBy(-_.weight)
		// ASSIGN FiLo dependencies
		services.withFilter(isFiLo).foreach { parent =>
			services.withFilter(!isFiLo(_)).foreach(child => Dependencies.add(parent, child))
		}
		postTik()
		// DEFs
		def isFiLo(s: AppService) = s.isInstanceOf[FirstInLastOutFeature]
	}
	def stop(): Unit = {
		stopping = true
		// reorder services in order of dependencies (parents after children)
		services = mutable.LinkedHashSet[AppService]() ++ services.toSeq.sortBy(_.weight)
		// mark state STOP but wait all services to be ACTIVE
		cancelStart()
		timeoutMs = deviceNow + App.config.timeoutDelay
		postTik()
		// DEFs
		def cancelStart() = services foreach (s => if (s.context == this && s.state < ACTIVE) s.cancelStart())
	}
	def onVisible(yes: Boolean): Unit = services foreach (s => if (s.context == this && s.state == ACTIVE) TryNLog { s.onUiVisible(yes) })


	/* INTERNAL API */

	def wakeup() = if (delayNextMs - deviceNow > 100) {delayNextMs = 0; postTik() }
	protected def postTik(delayMs: Long = 0): Unit = post("TIK", delayMs) { nextState() }
	protected def clearTik(): Unit = {
		delayNextMs = Long.MaxValue
		asyncExecContext.clear()
	}

	protected def nextState(): Unit = {
		/* TODO remove */
		val t0 = deviceNow

		var totalN, startedN = 0
		var changed = false
		//
		services foreach { s =>
			if (s.context == this) {
				if (s.state != s.nextState) changed = true
				totalN += 1
				if (s.state == ACTIVE && !s.forceStop) startedN += 1
			}
		}
		services foreach { s =>
			if (s.context == this && s.state == FINALIZED) unregisterService(s)
		}
		//
		val finalized = totalN == 0
		val started = !finalized && startedN == totalN
		//
		if (finalized) onFinalized()
		else if (stopping && isTimeout) onTimeout()
		//
		if (finalized || (started && !stopping)) clearTik()
		else postTik(nextDelay)
		logv("tikState", s"stopping=$stopping; all= $totalN;  total= $totalN;  started= $startedN;  time= ${deviceNow - t0 }")
		//
		// DEFs
		//
		def onFinalized() = {
			// for shared (with other context) services if any
			services foreach unregisterService
			postFinalize()
			serviceMgr.onFinalized
		}
		def isTimeout = if (timeoutMs > 0 && deviceNow > timeoutMs) {timeoutMs = 0; true } else false
		def onTimeout() = services foreach (s => if (s.context == this) s.timeout())
		def nextDelay: Long = {
			if (changed) {delayCounter = 0; delayStartMs = deviceNow }
			val delay = if (delayCounter < 4) {delayCounter += 1; 50 * delayCounter }
			else {
				val mult = if (stopping) 2 else 1
				val time = deviceNow - delayStartMs
				if (time < 2000) 250 * mult
				else if (time < 10000) 1000 * mult
				else if (time < 60000) 4000 * mult
				else 10000
			}
			delayNextMs = deviceNow + delay
			delay
		}
	}

}
