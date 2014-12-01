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

///** [[AppService]] that extends it can be cooled down if not used for some period. */
//trait CoolDownFeature {self: AppService =>}

/** [[AppService]] that extends it starts in first turn and stops last. */
trait FirstInLastOutFeature {
	self: AppService =>
}

/** [[AppService]] that extends it ensures that it can start right after init.
That is isStarted is called sequentially right after onInitialize, and if true is returned onStart is called. And service is considered Accessible. */
trait HotStartFeature {
	self: AppService =>
}



trait AppService extends AsyncExecContextInitializer with ActiveStateWatcher with Loggable {
	
	import ServiceState._
	type Config <: AnyRef
	implicit protected[app] var context: AppServiceContext = _
	implicit val thisService: AppService = this
	lazy protected[app] val contexts = collection.mutable.Set[Int]().empty
	protected var configOpt: Option[Config] = None
	var ID: String = getClass.getSimpleName
	private var _state: ServiceState.Value = _
	private[this] var _started, _stopped: Try[Boolean] = Success(false)
	protected[app] var startCanceled: Boolean = _
	protected[app] var stopTimeout: Boolean = _
	protected[app] var failureOpt: Option[Throwable] = None
	lazy protected[app] val activeWatchers = collection.mutable.WeakHashMap[ActiveStateWatcher, Boolean]()
	protected[app] var weight: Int = 0
	protected[app] var forceStop = false
	def isFailed = failureOpt.nonEmpty
	def state: ServiceState.Value = _state
	protected def state_=(v: ServiceState.Value): Unit = {
		logw("STATE", s"${" " * (90 - TAG.name.length) } [$ID: ${context.hashCode.toString.takeRight(3) }]:  ${_state } >  $v")
		_state = v
	}
	
	
	/* OVERRIDE */
	
	def stateInfo(): String = ""
	
	/* Lifecycle triggers to OVERRIDE */
	
	protected def onInitialize(): Unit = ()
	protected def onStart(): Unit = isStarted = Success(true)
	protected def onStartCancel(): Unit = ()
	protected def isStarted: Try[Boolean] = _started
	protected final def isStarted_=(value: Try[Boolean]): Unit = {
		_started = value
		context.wakeup()
	}
	protected[app] def onUiVisible(yes: Boolean): Unit = ()
	protected def onStop(): Unit = isStopped = Success(true)
	protected def onStopTimeout(): Unit = ()
	protected def isStopped: Try[Boolean] = _stopped
	protected final def isStopped_=(value: Try[Boolean]): Unit = {
		_stopped = value
		context.wakeup()
	}
	protected def onFinalize(): Unit = ()
	/** Called when dependency parent is failed.
	  @return true if service should fail.
	  */
	protected def onDependencyStartFailed(parent: AppService): Boolean = false

	
	
	
	/* INTERNAL API */
	def register(id: String = null)(implicit _context: AppServiceContext): this.type = {
		if (contexts add _context.hashCode()) {
			val oldContext = context
			context = _context
			// CASE: repeated registration of same service instance.
			// CAUSE: parallel service context started
			if (oldContext != null) {
				// reset state in case of repeated registration
				state = _state match {
					case null | FINALIZED => INIT // reinit
					case STOP => TryNLog {
						onStopTimeout()
						isStopped match {
							case Success(true) => INITED // restart
							case _ => onFinalize(); INIT // reinit
						}
					}.getOrElse { TryNLog { onFinalize() }; INIT }
					case STOPPED => INITED // restart
					case _ => _state
				}
				//
				// remove child dependencies to let old parent stop
				// WARN: the problem can arise when parent is not shared and service is started. After switch context it tries to use parent from new context which may be not yet started.
				Dependencies.remove((parent, child) => child == this)
			}
			else state = INIT
			//
			startCanceled = false
			stopTimeout = false
			failureOpt = None
			_started = Success(false)
			_stopped = Success(false)
			forceStop = false
			context.registerService(this)
		}
		if (id != null) ID = id
		this
	}
	def config(conf: Config): this.type = {
		configOpt = Option(conf)
		this
	}
	def dependsOn(services: AppService*)(implicit context: AppServiceContext): this.type = {
		services.foreach { s =>
			s.register() // no way to forget register
			Dependencies.add(s, this)
		}
		this
	}
	def watch(services: AppService*)(implicit context: AppServiceContext): this.type = {
		services.foreach { s =>
			s.register() // no way to forget register
			activeWatchers += (s -> true)
		}
		this
	}
	def unregister() = {
		forceStop = true
		context.wakeup()
	}
	protected[app] def cancelStart(): Unit = if (state == START) TryNLog { startCanceled = true; onStartCancel() }
	protected[app] def timeout(): Unit = if (state < STOPPED) TryNLog { stopTimeout = true; onStopTimeout() }
	protected[app] def onUnregistered(implicit cxt: AppServiceContext): Unit = {
		contexts.remove(cxt.hashCode())
		Dependencies.remove((parent, child) => parent == this || child == this)
	}
	//	protected def isShared: Boolean = contexts.size > 1
	//	protected[app] def garbage = contexts.isEmpty
	override def toString: String = s"[$ID]"




	/* STATE MACHINE */

	protected[app] final def nextState: ServiceState.Value = {
		//
		// EXECUTE
		//
		state match {
			case INIT => initialise
			case INITED => start
			case START => started
			case ACTIVE => stop
			case STOP => stopped
			case STOPPED => finalise
			case FINALIZED =>
			case _ => loge(msg = s"AppService $ID seems not registered: ")
		}
		//
		// DEFs
		//
		def initialise = trying {
			state = INITED
			preInitialize()
			onInitialize()
			if (isInstanceOf[HotStartFeature]) {
				isStarted match {case Success(true) => start; case _ => }
			}
		}
		def start = if (parentsAlive && parentsActive) trying {
			state = START
			onStart()
			started
		}
		def parentsAlive = {
			Dependencies.withParents(_.state > ACTIVE) { parent =>
				if (onDependencyStartFailed(parent)) fail(DependencyException(parent.ID, ID))
			}
			!isFailed
		}
		def parentsActive = Dependencies.hasNoParent { _.state != ACTIVE }
		def started: Boolean = trying {
			isStarted match {
				case Success(true) => state = ACTIVE
				case Success(false) if stopTimeout => fail(TimeoutException)
				case Failure(ex) => fail(ex)
				case _ =>
			}
			triggerActiveState()
			state == ACTIVE
		}
		def stop = if (mayStop && childrenStopped) trying {
			triggerActiveState(beforeStop = true)
			state = STOP
			onStop()
			stopped
		}
		def mayStop = context.stopping || forceStop
		def childrenStopped = Dependencies.hasNoChild { _.state < STOPPED }
		def stopped: Boolean = trying {
			isStopped match {
				case Success(true) => state = STOPPED
				case Success(false) if stopTimeout => fail(TimeoutException)
				case Failure(ex) => fail(ex)
				case _ =>
			}
			state == STOPPED
		}
		def finalise = trying {
			state = FINALIZED
			trying { onFinalize() }
			context = null
			configOpt = None
			activeWatchers.clear()
			postFinalize()
		}
		def fail(err: Throwable) = {
			loge(err, s"AppService [$ID] in state [$state] failed. ")
			if (!isFailed) {
				failureOpt = Option(err)
				val prevState = state
				if (prevState == START || prevState == ACTIVE) trying(onStop())
				if (prevState < FINALIZED) finalise
				if (prevState < ACTIVE) context.onServiceStartFailed(this, err)
			}
		}
		def trying[T](code: => T): T = try {code} catch {case err: Throwable => fail(err); null.asInstanceOf[T] }
		//
		state
	}
	
	
	
	
	/* ACTIVE STATE WATCHING */

	/** Override if watch any service active state. */
	override def onServiceActive(service: AppService, justBeforeStop: Boolean): Boolean = true

	/** Service functionality should be wrapped in this method to avoid access to service while it is not started. */
	def ifActive[R](code: => R): Try[R] =
		if (state == ACTIVE) try {Success(code)} catch {case e: Throwable => Failure(e) }
		else Failure(ServiceNotActiveException(ID, state.toString))
	
	/** Registers watcher of [[ACTIVE]] state change of service.
	@param watcher receives onServiceStarted event
	  */
	def watchActiveState(watcher: ActiveStateWatcher): Unit = if (state <= ACTIVE) {
		activeWatchers += (watcher -> true)
		triggerActiveState(false, watcher)
	}
	
	/** Triggers [[ACTIVE]] state change of service. */
	protected def triggerActiveState(beforeStop: Boolean = false, specific: ActiveStateWatcher = null): Unit = if (state == ACTIVE) {
		val watchers = if (specific == null) activeWatchers.map(_._1) else List(specific)
		watchers foreach { w =>
			val keepWatching = Try { w.onServiceActive(this, beforeStop) }.getOrElse(false)
			if (!keepWatching || beforeStop) activeWatchers -= w
		}
	}
}





/* SERVICE AVAILABILITY WATCHER */
trait ActiveStateWatcher {
	/** Is called when Service started (state = STARTED) or inaccessible (state >= STOP).
	  * @param service Service that state is watched
	  * @param  justBeforeStop true - if service is just before stopping. It's last chance to use it; false - if just started
	  * @return  true - to keep watching ; false - to stop watching
	  */
	def onServiceActive(service: AppService, justBeforeStop: Boolean): Boolean
}
