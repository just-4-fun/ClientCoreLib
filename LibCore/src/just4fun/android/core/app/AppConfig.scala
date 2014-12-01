package just4fun.android.core.app

import java.util.concurrent.ThreadPoolExecutor

trait AppConfig {
	/**
	Override in subclass to keep app operating after activity is finished.
	  Default is stopping app after activity finished (after back key pressed).
	  * @return true - if app should  continue operating. false otherwise.
	  */
	var liveAfterStop: Boolean = false
	/**
	Defines whether app waits current Instance to fully exit before init new one
	or spawns new Instance after current instance stopped and new launch occurred.
	  If <code>keepAliveAfterStop</code> is true <code>singleInstance</code> should also be true.
	  */
	var singleInstance: Boolean = false
	/**
	  If specific [[ThreadPoolExecutor]] that is used to execute async code is required.
	  @see [[just4fun.android.core.async.NewThreadContext]]
	  */
	def threadPoolExecutor_= (tpe: ThreadPoolExecutor) = just4fun.android.core.async.threadPoolExecutor = tpe
	/*syntax stub*/ private[this] def threadPoolExecutor = null
	/**
	 */
	var timeoutDelay = 60000
}
