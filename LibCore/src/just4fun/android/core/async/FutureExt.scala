package just4fun.android.core.async

import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal
import java.util.concurrent.CancellationException


case class FutureExt[T](context: AsyncExecContext, id: Any) {
	val promise = Promise[T]()
	var runnable: AsyncRunnable = _
	var canceled = false

	def onCompleteInUi[U](f: Try[T] => U): Unit = promise.future.onComplete(f)(UiThreadContext)
	def onSuccessInUi[U](pf: PartialFunction[T, U]): Unit = promise.future.onSuccess(pf)(UiThreadContext)
	def onFailureInUi[U](pf: PartialFunction[Throwable, U]): Unit = promise.future.onFailure(pf)(UiThreadContext)


	def cancel() = if (!canceled) {
		canceled = true
		context.cancel(runnable)
	}

	def thatRuns(body: => T): FutureExt[T] = {
		construct(body)
	}
	def thatRunsCancellable(bodyWithCancelCheck: (() => Unit) => T): FutureExt[T] = {
		construct(bodyWithCancelCheck(checkIsCanceled))
	}

	private def checkIsCanceled(): Unit = if (canceled) throw new CancellationException
	private def construct(body: => T): FutureExt[T] = {
		runnable = new AsyncRunnable(id) {
			override def run(): Unit = {
				promise complete {
					try Success(body) catch {case NonFatal(e) => Failure(e)}
				}
			}
		}
		this
	}

}



/* ASYNC RUNNABLE */

abstract class AsyncRunnable(val id: Any) extends Runnable
