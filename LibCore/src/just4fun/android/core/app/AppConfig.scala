package just4fun.android.core.app

import java.util.concurrent.ThreadPoolExecutor

import android.app.Notification
import android.app.Notification.Builder
import android.content.Intent
import just4fun.android.core.R
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
	var liveWithoutUi: Boolean = false
	
	/** If true calls [[android.os.Process]] killProcess when app is exited. */
	var killProcessAfterExit: Boolean = true //todo default false

	/* FOREGROUND behavior */
	/** Foreground [[android.app.Service]] mode defines when [[SurvivalAgent]] to call in startForeground and show status notification. ON_HIDE_UI: when activity is hidden, destroyed or none. ON_NO_UI: when activity is destroyed or none,  */
	var foregroundMode = ForegroundMode.NO_UI
	protected[app] def foregroundNotification(): Notification = {
		new Builder(App.context).setContentTitle("Title").setContentText("Text").setSmallIcon(R.drawable.ic_launcher).setOngoing(true).build()
	}


	/**
	  */
	var timeoutDelay = 60000


	/**
	Specific [[ThreadPoolExecutor]] to execute async code in [[just4fun.android.core.async.NewThreadContext]]
	  */
	val threadPoolExecutor: ThreadPoolExecutor = DefaultThreadPoolExecutor()


	/**
	  */
	protected[app] def onUtilize(): Unit = ()


	/** THE PLACE TO REGISTER SERVICES */
	protected[app] def onRegisterServices(implicit context: AppServiceContainer): Unit

}
