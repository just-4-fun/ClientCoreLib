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
	import ServiceOperation._
	var ID: String = getClass.getSimpleName
	implicit val thisService: AppService = this
	implicit protected[app] var container: AppServiceContainer = _
	private[this] var _reinitContainer: AppServiceContainer = _
	private[this] var _phase: ServicePhase.Value = NONE
	private[this] var _operation: ServiceOperation.Value = NORMAL
	private[this] var _started, _stopped, _resumed: Boolean = false
	private[this] var _failure: Throwable = null
	private[this] var _crucial: Boolean = false
	private[app] var weight: Int = 0
	lazy protected[app] val watchers = collection.mutable.WeakHashMap[ActivePhaseWatcher, Boolean]()

	
	
	/* OVERRIDE */
	
	def stateInfo(): String = s"id= $ID;  ph= $phase;  op= $operation;  wg= $weight"
	
	/* Lifecycle triggers to OVERRIDE */
	
	protected def onInitialize(): Unit = ()
	protected def onStart(): Unit = setStarted()
	protected def isStarted(isOk: Boolean = isOperationOk): Boolean = _started
	protected def onUiResumed(): Unit = ()
	protected def onUiPaused(): Unit = ()
	protected def onStop(isOk: Boolean = isOperationOk): Unit = setStopped()
	protected def isStopped(isOk: Boolean = isOperationOk): Boolean = _stopped
	protected def onUtilize(isOk: Boolean = isOperationOk): Unit = ()
	protected def onOperationChange(): Unit = ()
	protected def onError(err: Throwable, fatal: Boolean = false): Boolean = true


	/* PUBLIC API */

	def register(id: String = null)(implicit _container: AppServiceContainer): this.type = if (_container.isServing) {
		if (container != _container) {
			// if shared service reinit
			if (container != null) {
				nextPhase()
				if (phase <= ACTIVE || phase == UTILIZED) {
					container = _container
					_operation = NORMAL
					if (phase == NONE || phase == UTILIZED) phase = INIT
					else Dependencies.remove((parent, child) => child == this)
					if (phase == ACTIVE && !_resumed && App.control.isUiVisible) onVisibilityChange(true)
					container.register
				}
				else {
					Dependencies.remove((parent, child) => parent == this || child == this)
					_reinitContainer = _container
					_reinitContainer.register
				}
			}
			else {
				container = _container
				phase = INIT
				container.register
			}
		}
		if (id != null) ID = id
		this
	} else {
		loge(msg = s"Service [$ID: ${_container.name }] can't be registered in finishing container.")
		this
	}
	def unregister(wakeContainer: Boolean = true) = {
		if (_resumed) onVisibilityChange(false)
		_reinitContainer = null
		setFinishing()
		if (container != null && wakeContainer) container.wakeup()
	}


	/* CONFIGURATION */

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
			addActivePhaseWatcher(s)
		}
		this
	}
	def playsCrucialRole: this.type = {_crucial = true; this}


	/* INTERNAL API */
	protected final def setStarted(): Unit = {
		_started = true
		if (container != null) container.wakeup()
	}
	protected final def setStopped(): Unit = {
		_stopped = true
		if (container != null) container.wakeup()
	}

	def phase: ServicePhase.Value = _phase
	protected def phase_=(v: ServicePhase.Value): Unit = {
		logw("PHASE ", s"${" " * (90 - TAG.name.length) } [$ID: ${container.name }]:  ${_phase } >  $v")
		_phase = v
	}
	final def operation: ServiceOperation.Value = _operation
	protected def operation_=(v: ServiceOperation.Value) = {
		logw("OPER...", s"${" " * (90 - TAG.name.length) } [$ID: ${container.name }]:   >>>> ${v }")
		_operation = v
	}
	final def isStarting = phase < ACTIVE && _operation == NORMAL
	final def isServing = phase == ACTIVE && _operation == NORMAL
	final def isOperationOk: Boolean = if (phase < ACTIVE) operation == NORMAL else operation <= FINISHING
	final def isFinishing: Boolean = _operation == FINISHING
	private[app] def setFinishing() = if (_operation < FINISHING) {
		operation = FINISHING
		TryNLog { onOperationChange() }
	}
	final def isInterrupting: Boolean = _operation == INTERRUPTING
	private[app] def setInterrupting(): Unit = if (_operation < INTERRUPTING) {
		operation = INTERRUPTING
		TryNLog { onOperationChange() }
	}
	final def isFailed: Boolean = _failure != null
	final def failure: Option[Throwable] = Option(_failure)
	final def setFailure(err: Throwable, fatal: Boolean = false) = if (err != null && (TryNLog{onError(err, fatal)}.getOrElse(true) || fatal)) {
		_failure = err
		operation = FAILED
		TryNLog { onOperationChange() }
		if (container != null) container.wakeup()
	}
	final def isReinitializing = _reinitContainer != null
	def isCrucial: Boolean = _crucial

	protected[app] def onVisibilityChange(visible: Boolean): Unit = if (isServing) TryNLog {
		_resumed = visible
		logw("PHASE ", s"${" " * (90 - TAG.name.length) } [$ID: ${container.name }]:  RESUMED ? ${_resumed }")
		if (_resumed) onUiResumed() else onUiPaused()
	}

	override def toString: String = ID


	/* STATE MACHINE */

	protected[app] final def nextPhase(): Boolean = {
		val prevPhase = _phase
		_phase match {
			case INIT => initialise
			case INITED => start
			case START => started
			case ACTIVE => stop
			case STOP => stopped
			case STOPPED => utilize
			case NONE | UTILIZED =>
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
		def parentsStarted = Dependencies.hasNoParent { _.phase != ACTIVE }
		def start = if (!isOperationOk) utilize
		else if (parentsStarted) trying {
			phase = START
			onStart()
			started
		}
		def started: Boolean = trying {
			if (isStarted()) phase = ACTIVE
			else if (isInterrupting) throw TimeoutException
			if (phase == ACTIVE) {
				triggerActivePhase()
				if (App.control.isUiVisible) onVisibilityChange(true)
				true
			}
			else false
		}
		def childrenStopped = Dependencies.hasNoChild { _.phase < STOPPED }
		def stop = if (!isOperationOk || (isFinishing && childrenStopped)) trying {
			triggerActivePhase(beforeStop = true)
			phase = STOP
			onStop()
			stopped
		}
		def stopped: Boolean = trying {
			if (isStopped()) phase = STOPPED
			else if (isInterrupting) throw TimeoutException
			phase == STOPPED
		}
		def trying[T](code: => T): T = try {if (isFailed) throw _failure else code} catch {
			case err: Throwable =>
				loge(err, s"AppService [$ID] in state [$phase] failed. ")
				setFailure(err, true)
				if (phase < ACTIVE) {
					Dependencies.foreach { case (p, c) => if (p == this) TryNLog { c.setFailure(DependencyException(ID, c.ID)) } }
					triggerActivePhase()
					container.onStartFailed(_failure)
				}
				utilize
				null.asInstanceOf[T]
		}
		def utilize = {
			phase = UTILIZED
			TryNLog { onUtilize() }
			TryNLog { postUtilize() }
			// if shared service restarting
			if (isReinitializing && _reinitContainer.isServing) {
				container = _reinitContainer
				phase = INIT
				container.wakeup()
			}
			else {
				container = null
				weight = 0
				Dependencies.remove((parent, child) => parent == this || child == this)
			}
			_reinitContainer = null
			_operation = NORMAL
			_resumed = false
			_started = false
			_stopped = false
			_failure = null
		}
		//
		_phase != prevPhase
	}
	
	
	
	
	/* ACTIVE STATE WATCHING */

	/** Service functionality should be wrapped in this method to avoid access to service while it is not started. */
	def ifActive[R](code: => R): Try[R] =
		if (phase == ACTIVE) try {Success(code)} catch {case e: Throwable => Failure(e) }
		else Failure(ServiceNotActiveException(ID, phase.toString))
	
	/** Registers watcher of [[ACTIVE]] state change of service.
	@param watcher receives onServiceStarted event
	  */
	def addActivePhaseWatcher(watcher: ActivePhaseWatcher): Unit = if (phase <= ACTIVE) {
		watchers += (watcher -> true)
		if (phase == ACTIVE) triggerActivePhase(false, watcher)
	}
	
	/** Triggers [[ACTIVE]] state change of service. */
	protected def triggerActivePhase(beforeStop: Boolean = false, specific: ActivePhaseWatcher = null): Unit = {
		val ws = if (specific == null) watchers.map(_._1) else List(specific)
		if (isFailed) ws.foreach(s => TryNLog { s.onServiceFailed(this) })
		else if (beforeStop) ws.foreach(s => TryNLog { s.onServiceBeforeStop(this) })
		else ws.foreach(s => TryNLog { s.onServiceAfterStart(this) })
		//
		if ((beforeStop || isFailed) && specific == null) watchers.clear()
	}
}





/* SERVICE AVAILABILITY WATCHER */
trait ActivePhaseWatcher {
	/** Tracks whether Service ACTIVE phase fails, begins or ends.
	  * @param service Service that state is watched
	  */
	def onServiceAfterStart(service: AppService): Unit = ()
	def onServiceBeforeStop(service: AppService): Unit = ()
	def onServiceFailed(service: AppService): Unit = ()
}
