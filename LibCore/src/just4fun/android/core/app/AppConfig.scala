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
	  */
	var timeoutDelay = 60000

	/**
	  If app [[liveAfterStop]] = true, it could work as daemon [[android.app.Service]] after activity was destroyed. This service can be killed by the system and recreated later. In this case If [[resumeAfterKill]] is true, app services will be started as soon as service is recreated.
	  WARN: Be judicious not abusing user and exit app when service will have its work done.
	  */
	var resumeAfterKill: Boolean = false

	/**
	  If specific [[ThreadPoolExecutor]] that is used to execute async code is required.
	  @see [[just4fun.android.core.async.NewThreadContext]]
	  */
	def threadPoolExecutor_= (tpe: ThreadPoolExecutor) = just4fun.android.core.async.threadPoolExecutor = tpe
	/*syntax stub*/ private[this] def threadPoolExecutor = null
}
