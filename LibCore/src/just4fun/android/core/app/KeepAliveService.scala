package just4fun.android.core.app

import android.app.Service
import project.config.logging.Logger._
import android.content.{Context, Intent}
import android.os.IBinder


class KeepAliveService extends Service {
	import KeepAliveService._

	override def onCreate(): Unit = {
		daemon = this
		logv("onCreate", s"")
	}
	override def onDestroy(): Unit = logv("onDestroy", s"")
	override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
		val action = if (intent != null) intent.getAction else ""
		logv("onStartCommand", s"intent: $action")
		Service.START_STICKY
	}
	override def onBind(intent: Intent): IBinder = null
}


/* KEEP ALIVE */

private[app] object KeepAliveService extends Loggable {

	var daemon: KeepAliveService = _
	logv("PREINIT", s"")

	def initialize(appContext: Context): Unit = {
		logv("initialize", s"daemon= $daemon")
		val intent = new Intent(appContext, classOf[KeepAliveService])
		appContext.startService(intent)
	}
	def finalise(): Unit = if (daemon != null) {
		logv("finalise", s"")
		daemon.stopSelf()
		daemon = null
	}
}


