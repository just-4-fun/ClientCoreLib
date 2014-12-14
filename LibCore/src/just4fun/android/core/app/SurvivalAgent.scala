package just4fun.android.core.app

import android.app.Service
import android.content.Intent
import android.os.IBinder
import project.config.logging.Logger._

import scala.ref.WeakReference

object ForegroundMode extends Enumeration {
	val NEVER, NO_UI, HIDDEN_UI, ALWAYS = Value
}


private object SurvivalAgent {
	private var service: WeakReference[SurvivalAgent] = WeakReference(null)
	private var _starting = false
	private var _foreground = false
	private val _id = 0x10
	
	
	def isAlive = service.get.nonEmpty
	def isStarting = _starting && service.get.isEmpty

	private def onCreate(s: SurvivalAgent): Unit = {
		service = new WeakReference(s)
		logv("SurvivalAgent", s"onCreate")
		if (App.control.inStandBy) {
			if (App.control.lastCrashed && App.config.liveWithoutUi) App.control.start()
			else s.stopSelf()
		}
		else if (_foreground) startForeground()
		_starting = false
	}
	def start(): Unit = if (!isAlive && !_starting) {
		logv("SurvivalAgent", s"start")
		_starting = true
		val intent = new Intent(App.context, classOf[SurvivalAgent])
		App.context.startService(intent)
	}
	private def onStarted(intent: Intent): Int = {
		val action = if (intent != null) intent.getAction else "RECREATED"
		logv("SurvivalAgent", s"onStart  action= $action;  wasKilled= ${App.control.lastCrashed }")
		// 'intent = null' indicates recreating after kill
		if (App.config.liveWithoutUi) Service.START_STICKY else Service.START_NOT_STICKY
	}
	def finish(): Unit = {
		logv("SurvivalAgent", s"finish")
		if (isAlive) {
			service.get.get.stopSelf()
			service = new WeakReference(null)
		}
		else onFinished()
	}
	private def onFinished(): Unit = {
		logv("SurvivalAgent", s"onDestroy")
		_foreground = false
		_starting = false
		service = new WeakReference(null)
	}
	
	def foreground: Boolean = _foreground
	def foregroundRequired: Boolean = {
		import ForegroundMode._
		val mode = App.config.foregroundMode
		mode == ALWAYS || (mode == HIDDEN_UI && !App.control.isUiVisible) || (mode == NO_UI && !App.control.isUiAvailable)
	}
	def checkForeground(): Unit = if (_foreground != foregroundRequired) {
		_foreground = !_foreground
		logv("SurvivalAgent", s"Foreground   SET  ${!_foreground };  alive ? $isAlive")
		if (isAlive) if (_foreground) startForeground() else stopForeground()
		else if (_foreground) start()
	}
	private def startForeground() = {
		logv("SurvivalAgent", s"Foreground   START")
		service().startForeground(_id, App.config.foregroundNotification())
	}
	private def stopForeground() = {
		logv("SurvivalAgent", s"Foreground   STOP")
		service().stopForeground(true)
	}

}


class SurvivalAgent extends Service {
	override def onCreate(): Unit = SurvivalAgent.onCreate(this)
	override def onDestroy(): Unit = SurvivalAgent.onFinished()
	override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = SurvivalAgent.onStarted(intent)
	override def onBind(intent: Intent): IBinder = null
}