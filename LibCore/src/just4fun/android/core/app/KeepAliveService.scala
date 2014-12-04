package just4fun.android.core.app

import android.app.Service
import just4fun.android.core.persist.IntVar
import project.config.logging.Logger._
import android.content.{Context, Intent}
import android.os.IBinder

import scala.ref.WeakReference


class KeepAliveService extends Service {
	import KeepAliveService._

	override def onCreate(): Unit = {
		daemon = WeakReference(this)
		logv("onCreate", s"")
	}
	override def onDestroy(): Unit = {
		logv("onDestroy", s"")
		daemon = WeakReference(null)
	}
	override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
		val action = if (intent != null) intent.getAction else "RECREATED"
		logv("onStartCommand", s"intent: $action;  wasKilled= ${App.wasKilled}")
		// 'intent = null' indicates recreating after kill
		if (!App.isActive && App.wasKilled && intent == null && App.config.resumeAfterKill) App.start()
		if (App.config.resumeAfterKill) Service.START_STICKY else Service.START_NOT_STICKY
	}
	override def onBind(intent: Intent): IBinder = null
}


/* KEEP ALIVE */

private[app] object KeepAliveService extends Loggable {

	var daemon: WeakReference[KeepAliveService] = WeakReference(null)

	def initialize(appContext: Context): Unit = if (daemon.get.isEmpty) {
		logv("initialize", s"")
		val intent = new Intent(appContext, classOf[KeepAliveService])
		appContext.startService(intent)
	}
	def finalise(): Unit = if (daemon.get.isDefined) {
		logv("finalise", s"")
		daemon().stopSelf()
		daemon = WeakReference(null)
	}
}


