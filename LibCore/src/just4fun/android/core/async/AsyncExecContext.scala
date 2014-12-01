package just4fun.android.core.async

import scala.concurrent.ExecutionContext
import android.os._
import project.config.logging.Logger._
import java.util.concurrent.{LinkedBlockingQueue, ThreadFactory, TimeUnit, ThreadPoolExecutor}
import java.util.concurrent.atomic.AtomicInteger
import just4fun.android.core.app.Initializer


/* INTERFACE */
trait AsyncExecContext extends ExecutionContext with Loggable {
	// USAGE
	def execute[T](id: Any, delayMs: Long = 0, replace: Boolean = true)(body: => T): FutureExt[T] = {
		val future = FutureExt(this, id) thatRuns body
		if (replace) cancel(id)
		prepare().execute(delayMs, id, future.runnable)
		future
	}
	def executeCancelable[T](id: Any, delayMs: Long = 0, replace: Boolean = true)(bodyWithCancelCheck: (() => Unit) => T): FutureExt[T] = {
		val future = FutureExt(this, id) thatRunsCancellable bodyWithCancelCheck
		if (replace) cancel(id)
		prepare().execute(delayMs, id, future.runnable)
		future
	}

	//ExecutionContext API
	override def reportFailure(t: Throwable): Unit = loge(t)
	//	def execute(r: Runnable): Unit
	override def prepare(): AsyncExecContext = this
	def execute(delay: Long, id: Any, r: Runnable): Unit
	def cancel(idORrunnable: Any): Unit
	def clear(): Unit
	def quit(softly: Boolean = false): Unit
	def isQuit: Boolean

}





/* HANDLER   implementation */
class HandlerContext(name: String, looper: Looper) extends AsyncExecContext {
	override lazy implicit val TAG = LogTag(name + "_H")
	private val QUIT = 0x011
	private var quit = false
	private lazy val threadName = Thread.currentThread.getName
	private lazy val isMainThread = looper == Looper.getMainLooper

	val handler: Handler = new Handler(looper) {
		override def dispatchMessage(msg: Message) {
			msg.getCallback match {
				case r: Runnable => handle(r)
				case null if msg.what == QUIT => quit()
				case _ => logw("ASYNC", s"$threadName  UNKNOWN:  what= ${msg.what }, token= ${msg.obj }")
			}
		}
	}
	def handle(runnable: Runnable) {
		val id = runnable match {
			case r: AsyncRunnable => if (r.id == null) null else r.id.toString
			case r if r.getClass.getSimpleName == "CallbackRunnable" => "callback"
			case _ => "future"
		}
		if (id != null) logv("ASYNC", s"[$threadName]: $id")
		runnable.run()
	}

	override def execute(r: Runnable): Unit = {
		handler.post(r)
	}
	override def execute(delay: Long, id: Any, r: Runnable): Unit = {
		handler.postAtTime(r, id, SystemClock.uptimeMillis() + delay)
	}
	override def cancel(idORrunnable: Any): Unit = idORrunnable match {
		case r: Runnable => handler.removeCallbacks(r)
		case id => if (id != null) handler.removeCallbacksAndMessages(id)
	}
	override def clear(): Unit = handler.removeCallbacksAndMessages(null)
	def quit(safely: Boolean = false) = if (!quit) {
		if (safely) handler.sendEmptyMessage(QUIT)
		else {
			quit = true
			if (isMainThread) clear() else looper.quit()
		}
	}
	override def isQuit: Boolean = quit
	//	override def reportFailure(t: Throwable): Unit = loge(t)
	//	override def prepare(): ExecutionContext = this
}






/* THREAD POOL CONTEXT */

/** [[threadPoolExecutor]] should be set in AppConfig before using this object. Else it will be set to [[DefaultThreadPoolExecutor]] */
object NewThreadContext extends ThreadPoolContext(threadPoolExecutor)



object DefaultThreadPoolExecutor {
	def apply(): ThreadPoolExecutor = {
		val cpus = Runtime.getRuntime.availableProcessors
		val corePoolSize = cpus + 1
		val maxPoolSize = cpus * 2 + 1
		val keepAlive = 1
		val factory = new ThreadFactory {
			private val threadNo = new AtomicInteger(1)
			def newThread(r: Runnable) = new Thread(r, "AsyncContext #" + threadNo.getAndIncrement)
		}
		val queue = new LinkedBlockingQueue[Runnable](128)
		new ThreadPoolExecutor(corePoolSize, maxPoolSize, keepAlive, TimeUnit.SECONDS, queue, factory)
	}
}



class ThreadPoolContext(var executor: ThreadPoolExecutor) extends AsyncExecContext {
	import scala.collection.JavaConverters._
	//
	if (executor == null) executor = DefaultThreadPoolExecutor()
	//
	val handler: Handler = new Handler(Looper.getMainLooper) {
		override def dispatchMessage(msg: Message) {
			msg.getCallback match {
				case r: Runnable => execute(r)
				case _ =>
			}
		}
	}
	override def execute(r: Runnable): Unit = executor.execute(r)
	override def execute(delay: Long, id: Any, r: Runnable): Unit = handler.postDelayed(r, delay)
	override def cancel(idORrunnable: Any): Unit = idORrunnable match {
		case r: Runnable => handler.removeCallbacks(r); executor.remove(r)
		case id => if (id != null) handler.removeCallbacksAndMessages(id)
	}
	override def isQuit: Boolean = executor.isShutdown
	override def quit(softly: Boolean = false): Unit = {
		handler.removeCallbacksAndMessages(null)
		if (softly) executor.shutdown() else executor.shutdownNow()
	}
	override def clear(): Unit = {
		handler.removeCallbacksAndMessages(null)
		executor.getQueue.asScala.toSeq.foreach(executor.remove)
	}
	//	override def reportFailure(t: Throwable): Unit = ???
	//	override def prepare(): ExecutionContext = this
}






/* UI CONTEXT */

/** Re-posts runnable if UI is reconfiguring */
object UiThreadContext extends HandlerContext("Main", Looper.getMainLooper) {
	override def handle(runnable: Runnable): Unit = {
		super.handle(runnable)
		// TODO make this code available
		//			if (App.isReconfiguring) handler.post(runnable)
		//			else super.handle(runnable)
	}
}
