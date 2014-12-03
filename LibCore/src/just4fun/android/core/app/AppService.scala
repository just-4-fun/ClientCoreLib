package just4fun.android.core.app

import just4fun.android.core.async.{AsyncExecContext, AsyncExecContextInitializer}
import just4fun.android.core.utils._
import project.config.logging.Logger._
import scala.Some
import scala.util.{Failure, Success, Try}



/** [[AppService]] that extends it runs its async operations in allocated parallel [[Thread]]. */
trait ParallelThreadFeature {
	self: AppService =>
}

/** [[AppService]] that extends it runs its async operations each in new [[Thread]] from pool. */
trait NewThreadFeature {
	self: AppService =>
}

/** [[AppService]] that extends it is inited before other services. So it can be used by service in init phase. */
trait FirstPriorityFeature {
	self: AppService =>
}



trait AppService extends AsyncExecContextInitializer with ActivePhaseWatcher with Loggable {
	
	import ServicePhase._
	import OperationStatus._
	type Config <: AnyRef
	implicit protected[app] var context: AppServiceContext = _
	implicit val thisService: AppService = this
	protected var config: Option[Config] = None
	var ID: String = getClass.getSimpleName
	private var _phase: ServicePhase.Value = null
	private[this] var _operation: OperationStatus.Value = OK
	private[this] var _failure: Throwable = null
	protected[app] var weight: Int = 0
	lazy protected[app] val activeWatchers = collection.mutable.WeakHashMap[ActivePhaseWatcher, Boolean]()
	private[this] var _started, _stopped: Boolean = false
	
	
	def phase: ServicePhase.Value = _phase
	protected def phase_=(v: ServicePhase.Value): Unit = {
		logw("PHASE ", s"${" " * (90 - TAG.name.length) } [$ID: ${context.hashCode.toString.takeRight(3) }]:  ${_phase } >  $v")
		_phase = v
	}
	
	def operation: OperationStatus.Value = _operation
	def failure: Option[Throwable] = Option(_failure)
	def failure_=(err: Throwable) = {
		_failure = err
		_operation = FAILED
		TryNLog { onOperationChange() }
	}
	final def isFailed: Boolean = _failure != null
	final def isStopping: Boolean = _operation == STOPPING
	final def setStopping() = {
		_operation = STOPPING
		TryNLog { onOperationChange() }
	}
	final def isTimedOut: Boolean = _operation == TIMEDOUT
	protected[app] def setTimedOut(): Unit = if (phase < STOPPED) {
		_operation = TIMEDOUT
		TryNLog { onOperationChange() }
	}
	protected final def setStarted(): Unit = {
		_started = true
		context.wakeup()
	}
	protected final def setStopped(): Unit = {
		_stopped = true
		context.wakeup()
	}


	/* OVERRIDE */
	
	def stateInfo(): String = ""
	
	/* Lifecycle triggers to OVERRIDE */
	
	protected def onInitialize(): Unit = ()
	protected def onStart(): Unit = setStarted()
	protected def isStarted(operationIsOK: Boolean = operation == OK): Boolean = _started
	protected def onStop(operationIsOK: Boolean = operation == OK): Unit = setStopped()
	protected def isStopped(operationIsOK: Boolean = operation == OK): Boolean = _stopped
	protected def onFinalize(operationIsOK: Boolean = operation == OK): Unit = ()
	protected[app] def onUiVisible(yes: Boolean): Unit = ()
	protected def onOperationChange(): Unit = ()
	protected def onDependencyStartFailed(implicit parent: AppService): Unit = {
		failure = DependencyException(parent.ID, ID)
	}

	
	
	
	/* INTERNAL API */
	def register(id: String = null)(implicit _context: AppServiceContext): this.type = {
		if (context != _context) {
			val sharedStart = context != null
			if (sharedStart) {
				if (phase <= ACTIVE && isStopping) _operation = OK
				else if (phase == STOP) setTimedOut()
			}
			nextPhase(sharedStart)(_context)
		}
		if (id != null) ID = id
		this
	}
	def config(conf: Config): this.type = {
		register()
		config = Option(conf)
		this
	}
	def dependsOn(services: AppService*)(implicit context: AppServiceContext): this.type = {
		register()
		services.foreach { s =>
			s.register() // no way to forget register
			Dependencies.add(s, this)
		}
		this
	}
	def watch(services: AppService*)(implicit context: AppServiceContext): this.type = {
		register()
		services.foreach { s =>
			s.register() // no way to forget register
			activeWatchers += (s -> true)
		}
		this
	}
	def unregister(wakeContext: Boolean = true) = {
		setStopping()
		if (wakeContext) context.wakeup()
	}
	override def toString: String = ID




	/* STATE MACHINE */

	protected[app] final def nextPhase(sharedStart: Boolean = false)(implicit _context: AppServiceContext): ServicePhase.Value = {
		phase match {
			case null | FINALIZED => register
			case INIT => initialise
			case INITED => start
			case START => started
			case ACTIVE => stop
			case STOP => stopped
			case STOPPED => finalise
		}
		//
		if (sharedStart) {
			if (phase <= ACTIVE) reset
			else if (phase < FINALIZED) finalise
			if (phase == FINALIZED) register
		}
		//
		// DEFs
		//
		def register = {
			context = _context
			phase = INIT
			context.registerService
		}
		def initialise = trying {
			phase = INITED
			preInitialize()
			onInitialize()
			if (isInstanceOf[FirstPriorityFeature]) start
		}
		def start = if (parentsStarted) trying {
			phase = START
			onStart()
			started
		}
		def parentsStarted = Dependencies.hasNoParent { _.phase != ACTIVE }
		def started: Boolean = trying {
			if (isStarted()) phase = ACTIVE
			else if (isTimedOut) throw TimeoutException
			if (phase == ACTIVE) {
				triggerActivePhase()
				if (context.isVisible) onUiVisible(true)
				true
			} else false
		}
		def stop = if (isStopping && childrenStopped) trying {
			triggerActivePhase(beforeStop = true)
			phase = STOP
			onStop()
			stopped
		}
		def childrenStopped = Dependencies.hasNoChild { _.phase < STOPPED }
		def stopped: Boolean = trying {
			if (isStopped()) phase = STOPPED
			else if (isTimedOut) throw TimeoutException
			phase == STOPPED
		}
		def trying[T](code: => T): T = try {if (isFailed) throw _failure else code} catch {
			case err: Throwable =>
				loge(err, s"AppService [$ID] in state [$phase] failed. ")
				failure = err
				if (phase < ACTIVE) {
					Dependencies.foreach { entry => if (entry._1 == this) TryNLog { entry._2.onDependencyStartFailed } }
					context.onServiceStartFailed(_failure)
				}
				finalise
				null.asInstanceOf[T]
		}
		def finalise = {
			phase = FINALIZED
			TryNLog { onFinalize() }
			recycle
		}
		def recycle = {
			TryNLog { postFinalize() }
			_operation = OK
			_started = false
			_stopped = false
			_failure = null
			config = None
			context = null
			weight = 0
			Dependencies.remove((parent, child) => parent == this || child == this)
			activeWatchers.clear()
		}
		def reset: Unit = {
			Dependencies.remove((parent, child) => child == this || (phase == ACTIVE && parent == this))
		}
		//
		phase
	}
	
	
	
	
	/* ACTIVE STATE WATCHING */

	/** Override if watch any service active state. */
	override def onServiceActive(service: AppService, justBeforeStop: Boolean): Boolean = true

	/** Service functionality should be wrapped in this method to avoid access to service while it is not started. */
	def ifActive[R](code: => R): Try[R] =
		if (phase == ACTIVE) try {Success(code)} catch {case e: Throwable => Failure(e) }
		else Failure(ServiceNotActiveException(ID, phase.toString))
	
	/** Registers watcher of [[ACTIVE]] state change of service.
	@param watcher receives onServiceStarted event
	  */
	def watchActivePhase(watcher: ActivePhaseWatcher): Unit = if (phase <= ACTIVE) {
		activeWatchers += (watcher -> true)
		triggerActivePhase(false, watcher)
	}
	
	/** Triggers [[ACTIVE]] state change of service. */
	protected def triggerActivePhase(beforeStop: Boolean = false, specific: ActivePhaseWatcher = null): Unit = if (phase == ACTIVE) {
		val watchers = if (specific == null) activeWatchers.map(_._1) else List(specific)
		watchers foreach { w =>
			val keepWatching = Try { w.onServiceActive(this, beforeStop) }.getOrElse(false)
			if (!keepWatching || beforeStop) activeWatchers -= w
		}
	}
}





/* SERVICE AVAILABILITY WATCHER */
trait ActivePhaseWatcher {
	/** Is called when Service started (state = STARTED) or inaccessible (state >= STOP).
	  * @param service Service that state is watched
	  * @param  justBeforeStop true - if service is just before stopping. It's last chance to use it; false - if just started
	  * @return  true - to keep watching ; false - to stop watching
	  */
	def onServiceActive(service: AppService, justBeforeStop: Boolean): Boolean
}
