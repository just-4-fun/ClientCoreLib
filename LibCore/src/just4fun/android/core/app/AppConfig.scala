package just4fun.android.core.app

import java.util.concurrent.ThreadPoolExecutor

import android.app.Notification
import just4fun.android.core.async.DefaultThreadPoolExecutor
import project.config.logging.Logger._

trait AppConfig {

//	/**
//	Defines whether app waits current Instance to fully exit before init new one
//	or spawns new Instance after current instance stopped and new launch occurred.
//	  If [[liveAfterStop]] is true [[singleInstance]] should also be true.
//	  */
//	var singleInstance: Boolean = false

	/**
	Set to true to keep app operating after activity is finished.
	Default is stopping app after activity finished (after back key pressed).
	  */
	var liveAfterStop: Boolean = false

	/**
	If app [[liveAfterStop]] = true, it could work as daemon [[android.app.Service]] after activity was destroyed. This service can be killed by the system and recreated later. In this case If [[resumeAfterKill]] is true, app services will be started as soon as service is recreated.
	  WARN: Be judicious not abusing user and exit app when service will have its work done.
	  */
	var resumeAfterKill: Boolean = false

	/* FOREGROUND behavior */
	/** Foreground [[android.app.Service]] mode defines when [[KeepAliveService]] to call in startForeground and show status notification. ON_HIDE_UI: when activity is hidden, destroyed or none. ON_NO_UI: when activity is destroyed or none,  */
	object ForegroundMode extends Enumeration {
		val NEVER, ON_NO_UI, ON_HIDE_UI, ALWAYS = Value
	}
	var foregroundMode = ForegroundMode.ON_NO_UI
	//TODO
	def foregroundNotification(): Notification = { ??? }


	/**
	  */
	var timeoutDelay = 60000


	/**
	Specific [[ThreadPoolExecutor]] to execute async code in [[just4fun.android.core.async.NewThreadContext]]
	  */
	val threadPoolExecutor: ThreadPoolExecutor = DefaultThreadPoolExecutor()


	/**
	  */
	def onExited(): Unit = ()


	/** THE PLACE TO REGISTER SERVICES */
	def onRegisterServices(implicit context: AppServiceContainer): Unit

	/**
	 @return true - if error is fatal and App should be set in FAILED state; false - if App should continue
	 */
	def isServiceStartFatalError(service: AppService, err: Throwable): Boolean = {
		logv("onServiceStartFailed", s"${service.ID };  Error: ${err.getMessage }")
		service match {
			case _ => false // decide fail App or not
		}
	}

}
