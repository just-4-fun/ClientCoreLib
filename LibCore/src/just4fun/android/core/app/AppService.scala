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
	implicit protected[app] var container: AppServiceContainer = _
	implicit val thisService: AppService = this
	var ID: String = getClass.getSimpleName
	private var _phase: ServicePhase.Value = NONE
	private[this] var _operation: OperationStatus.Value = NORMAL
	private[this] var _failure: Throwable = null
	protected[app] var weight: Int = 0
	lazy protected[app] val watchers = collection.mutable.WeakHashMap[ActivePhaseWatcher, Boolean]()
	private[this] var _started, _stopped, _resumed: Boolean = false

	
	
	/* OVERRIDE */
	
	def stateInfo(): String = ""
	
	/* Lifecycle triggers to OVERRIDE */
	
	protected def onInitialize(): Unit = ()
	protected def onStart(): Unit = setStarted()
	protected def isStarted(operationIsOK: Boolean = operation == NORMAL): Boolean = _started
	protected def onUiResumed(): Unit = ()
	protected def onUiPaused(): Unit = ()
	protected def onStop(operationIsOK: Boolean = operation <= FINISHING): Unit = setStopped()
	protected def isStopped(operationIsOK: Boolean = operation <= FINISHING): Boolean = _stopped
	protected def onFinalize(operationIsOK: Boolean = operation <= FINISHING): Unit = ()
	protected def onOperationChange(): Unit = ()
	protected def onDependencyStartFailed(implicit parent: AppService): Unit = {
		failure = DependencyException(parent.ID, ID)
	}

	
	
	
	/* INTERNAL API */
	def phase: ServicePhase.Value = _phase
	protected def phase_=(v: ServicePhase.Value): Unit = {
		logw("PHASE ", s"${" " * (90 - TAG.name.length) } [$ID: ${container.name }]:  ${_phase } >  $v")
		_phase = v
	}

	def operation: OperationStatus.Value = _operation
	def failure: Option[Throwable] = Option(_failure)
	def failure_=(err: Throwable) = if (_failure == null) {
		_failure = err
		_operation = FAILED
		TryNLog { onOperationChange() }
		if (container != null) container.wakeup()
	}
	final def isFailed: Boolean = _failure != null
	final def isFinishing: Boolean = _operation == FINISHING
	final def setFinishing() = if (_operation < FINISHING) {
		_operation = FINISHING
		TryNLog { onOperationChange() }
	}
	final def isTimedOut: Boolean = _operation == TIMEDOUT
	protected[app] def setTimedOut(): Unit = if (_operation < TIMEDOUT) {
		_operation = TIMEDOUT
		TryNLog { onOperationChange() }
	}
	protected final def setStarted(): Unit = {
		_started = true
		if (container != null) container.wakeup()
	}
	protected final def setStopped(): Unit = {
		_stopped = true
		if (container != null) container.wakeup()
	}

	override def toString: String = ID


	def register(id: String = null)(implicit _container: AppServiceContainer): this.type = if (!_container.finishing) {
		if (container != _container) {
			// if shared service restart
			if (container != null) {
				_operation = NORMAL
				nextPhase()
				if (phase == ACTIVE && !_resumed && App().isUiVsible) onVisibilityChange(true)
				if (phase <= ACTIVE) Dependencies.remove((parent, child) => child == this)
				else Dependencies.remove((parent, child) => parent == this || child == this)
			}
			container = _container
			if (phase == NONE || phase == FINALIZED) phase = INIT
			container.registerService
		}
		if (id != null) ID = id
		this
	} else {
		loge(msg = s"Service [$ID: ${_container.name }] can't be registered in finishing container.")
		this
	}
	def dependsOn(services: AppService*)(implicit container: AppServiceContainer): this.type = {
		register()
		services.foreach { s =>
			s.register() // no way to forget register
			Dependencies.add(s, this)
		}
		this
	}
	def watch(services: AppService*)(implicit container: AppServiceContainer): this.type = {
		register()
		services.foreach { s =>
			s.register() // no way to forget register
			watchers += (s -> true)
		}
		this
	}
	def unregister(wakeContainer: Boolean = true) = {
		setFinishing()
		if (container != null && wakeContainer) container.wakeup()
	}
	protected[app] def onVisibilityChange(visible: Boolean, force: Boolean = false): Unit = if (operation == NORMAL || force) TryNLog {
		_resumed = visible
		logw("PHASE ", s"${" " * (90 - TAG.name.length) } [$ID: ${container.name }]:  RESUMED ? ${_resumed }")
		if (_resumed) onUiResumed() else onUiPaused()
	}



	/* STATE MACHINE */

	protected[app] final def nextPhase(): Boolean = {
		val prevPhase = _phase
		_phase match {
			case INIT => initialise
			case INITED => start
			case START => started
			case ACTIVE => stop
			case STOP => stopped
			case STOPPED => finalise
			case NONE | FINALIZED =>
		}
		//
		// DEFs
		//
		def initialise = trying {
			phase = INITED
			preInitialize()
			onInitialize()
			if (isInstanceOf[FirstPriorityFeature]) start
		}
		def start = if (isFinishing || isTimedOut) finalise
		else if (parentsStarted) trying {
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
				if (App().isUiVsible) onVisibilityChange(true)
				true
			}
			else false
		}
		def stop = if (isFinishing && childrenStopped) trying {
			triggerActivePhase(beforeStop = true)
			if (_resumed) onVisibilityChange(false, force = true)
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
					Dependencies.foreach { case (p, c) => if (p == this) TryNLog { c.onDependencyStartFailed } }
					container.onServiceStartFailed(_failure)
				}
				finalise
				null.asInstanceOf[T]
		}
		def finalise = {
			TryNLog { onFinalize() }
			TryNLog { postFinalize() }
			// if shared service restart
			if (_operation == NORMAL) phase = INIT
			else {
				phase = FINALIZED
				_operation = NORMAL
				container = null
				weight = 0
				Dependencies.remove((parent, child) => parent == this || child == this)
				watchers.clear()
			}
			_resumed = false
			_started = false
			_stopped = false
			_failure = null
		}
		//
		_phase != prevPhase
	}
	
	
	
	
	/* ACTIVE STATE WATCHING */

	/** Override if watch any service active state. */
	override def onServiceActive(service: AppService, justBeforeStop: Boolean): Unit = ()

	/** Service functionality should be wrapped in this method to avoid access to service while it is not started. */
	def ifActive[R](code: => R): Try[R] =
		if (phase == ACTIVE) try {Success(code)} catch {case e: Throwable => Failure(e) }
		else Failure(ServiceNotActiveException(ID, phase.toString))
	
	/** Registers watcher of [[ACTIVE]] state change of service.
	@param watcher receives onServiceStarted event
	  */
	def watchActivePhase(watcher: ActivePhaseWatcher): Unit = if (phase <= ACTIVE) {
		watchers += (watcher -> true)
		if (phase == ACTIVE) triggerActivePhase(false, watcher)
	}
	
	/** Triggers [[ACTIVE]] state change of service. */
	protected def triggerActivePhase(beforeStop: Boolean = false, specific: ActivePhaseWatcher = null): Unit = {
		(if (specific == null) watchers.map(_._1) else List(specific)) foreach { w =>
			Try { w.onServiceActive(this, beforeStop) }
		}
	}
}





/* SERVICE AVAILABILITY WATCHER */
trait ActivePhaseWatcher {
	/** Is called when Service started (state = STARTED) or inaccessible (state >= STOP).
	  * @param service Service that state is watched
	  * @param  justBeforeStop true - if service is just before stopping. It's last chance to use it; false - if just started
	  */
	def onServiceActive(service: AppService, justBeforeStop: Boolean): Unit
}
