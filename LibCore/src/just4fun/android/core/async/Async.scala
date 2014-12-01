package just4fun.android.core.async

import scala.concurrent.{Promise, Future}

object Async {


	/* IMPLICITS */
	implicit def ext2future[T](f: FutureExt[T]): Future[T] = f.promise.future
	implicit def ext2promise[T](f: FutureExt[T]): Promise[T] = f.promise





	/* USAGE */

	def post[T](id: Any, delayMs: Long = 0, replace: Boolean = true)(body: => T)(implicit cxt: AsyncExecContext = null) = {
		val _cxt = if (cxt == null) UiThreadContext else cxt
		_cxt.execute[T](id, delayMs, replace)(body)
	}
	def postCancellable[T](id: Any, delayMs: Long = 0, replace: Boolean = true)(bodyWithCancelCheck: (() => Unit) => T)(implicit cxt: AsyncExecContext = null): FutureExt[T] = {
		val _cxt = if (cxt == null) UiThreadContext else cxt
		_cxt.executeCancelable[T](id, delayMs, replace)(bodyWithCancelCheck)
	}
	def cancelPost(id: Any)(implicit cxt: AsyncExecContext = null) = {
		val _cxt = if (cxt == null) UiThreadContext else cxt
		_cxt.cancel(id)
	}

	def postUI[T](id: Any, delayMs: Long = 0, replace: Boolean = true) = UiThreadContext.execute[T](id, delayMs, replace) _
	def postUICancelable[T](id: Any, delayMs: Long = 0, replace: Boolean = true)(bodyWithCancelCheck: (() => Unit) => T): FutureExt[T] = {
		UiThreadContext.executeCancelable[T](id, delayMs, replace)(bodyWithCancelCheck)
	}
	def cancelPostUI(id: Any) = UiThreadContext.cancel(id)

	def fork[T](id: Any, delayMs: Long = 0, replace: Boolean = true) = NewThreadContext.execute[T](id, delayMs, replace) _
	def forkCancellable[T](id: Any, delayMs: Long = 0, replace: Boolean = true)(bodyWithCancelCheck: (() => Unit) => T): FutureExt[T] = {
		NewThreadContext.executeCancelable[T](id, delayMs, replace)(bodyWithCancelCheck)
	}
	def cancelFork(id: Any) = NewThreadContext.cancel(id)

}
