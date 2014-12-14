package just4fun.android.core.async

import project.config.logging.Logger._
import android.os.{HandlerThread, Looper}
import just4fun.android.core.app.{NewThreadFeature, ParallelThreadFeature, Initializer}


trait AsyncExecContextInitializer extends Initializer {self: Loggable =>
	private[this] var _execCxt: AsyncExecContext = _
	implicit def asyncExecContext: AsyncExecContext = _execCxt
	abstract override def preInitialize() = {
		if (_execCxt == null || _execCxt.isQuit) _execCxt = this match {
			case _: ParallelThreadFeature => val thread = new HandlerThread(TAG.name); thread.start()
				new HandlerContext(TAG.name, thread.getLooper)
			case _: NewThreadFeature => NewThreadContext
			case _ => new HandlerContext(TAG.name, Looper.getMainLooper)
		}
		super.preInitialize()
	}
	abstract override def postUtilize() = {
		_execCxt.quit()
		_execCxt = null
		super.postUtilize()
	}
}
