package just4fun.android.core.inet

import android.content.{BroadcastReceiver, Context, Intent, IntentFilter}
import android.net.{NetworkInfo, ConnectivityManager => ConnMgr}
import just4fun.android.core.app._
import just4fun.android.core.async.Async._
import just4fun.android.core.async._
import just4fun.android.core.utils.TryNLog
import project.config.logging.Logger._

import scala.util.Try

/* COMPANION */

object InetService {
	private var _online = false
	//
	def online = _online;
}


/* CLASS */

abstract class InetService extends AppService with NewThreadFeature with FirstPriorityFeature {
	import just4fun.android.core.inet.InetService._
	import OperationStatus._
	ID = "INET"
	private val LONG_SPAN: Int = 60000
	private val SHORT_SPAN: Int = 4000
	lazy private val listeners = collection.mutable.Set[OnlineStatusListener]()
	private var receiver: BroadcastReceiver = _
	private var checkSpan: Int = LONG_SPAN
	private var future: FutureExt[Unit] = _

	/* USAGE */

	/**
	Parallel execution.
	  * @param opts
	  * @param canceled
	  * @return
	  */
	def loadString(opts: InetOptions, canceled: () => Boolean = () => false): FutureExt[String] =
		post("loadString") { loadStringSync(opts, canceled).get }
	def loadBytes(opts: InetOptions, canceled: () => Boolean = () => false): FutureExt[Array[Byte]] =
		post("loadBytes") { loadBytesSync(opts, canceled).get }
	/**
	Sequential execution.
	  * @param opts
	  * @param canceled
	  * @return
	  */
	def loadStringSync(opts: InetOptions, canceled: () => Boolean = () => false): Try[String] = Try {
		InetRequest(opts.clone, new StreamToString, canceled).execute().get }
	def loadBytesSync(opts: InetOptions, canceled: () => Boolean = () => false): Try[Array[Byte]] = Try {
		InetRequest(opts.clone, new StreamToBytes, canceled).execute().get }

	def addListener(lr: OnlineStatusListener, fireNow: Boolean = true) = {
		listeners += lr
		if (fireNow) TryNLog { lr.onlineStatusChanged(_online) }
	}
	def removeListener(lr: OnlineStatusListener) = listeners -= lr

	def isTypeAvailable(typ: Int) = { val info = connMgr.getNetworkInfo(typ); info != null && info.isAvailable }
	def isMobileAvailable: Boolean = isTypeAvailable(ConnMgr.TYPE_MOBILE) || isTypeAvailable(ConnMgr.TYPE_WIMAX)
	def getConnectionType: Int = {
		val netInfo: NetworkInfo = connMgr.getActiveNetworkInfo
		if (netInfo != null) netInfo.getType else -1
	}


	/* SERVICE API */
	override protected def onInitialize(): Unit = {
		receiver = new BroadcastReceiver {
			def onReceive(context: Context, intent: Intent) {
				val isOnline = isReallyOnline
				logv("onReceive", s"wasOnline: ${_online};  isOnline: $isOnline")
				if (_online && !isOnline) fireEvent(false)
				else if (!_online && isOnline) postCheck(SHORT_SPAN)
			}
		}
		App().registerReceiver(receiver, new IntentFilter(ConnMgr.CONNECTIVITY_ACTION))
		checkOnline()
	}
	override protected def onFinalize(operationIsOK: Boolean = operation == OK): Unit = {
		_online = false
		listeners.clear()
		TryNLog { App().unregisterReceiver(receiver) }
		receiver = null
		postCheckCancel()
		future = null
	}

	override def stateInfo() = s"online = ${_online}"

	/* INTERNAL API */
	private def postCheck(span: Int = 0) = {
		checkSpan = if (span == 0) checkSpan else span
		future = postUI("Check Online", checkSpan) { checkOnline() }
	}
	private def postCheckCancel() = if (future != null) future.cancel()
	private def connMgr = App().getSystemService(Context.CONNECTIVITY_SERVICE).asInstanceOf[ConnMgr]
	private def isReallyOnline: Boolean = {
		val netInfo: NetworkInfo = connMgr.getActiveNetworkInfo
		netInfo != null && netInfo.isConnected
	}
	private def checkOnline() {
		val isOnline = isReallyOnline
		if (isOnline != _online) fireEvent(isOnline)
		else if (!isOnline) postCheck()
	}
	private def fireEvent(isOnline: Boolean) {
		_online = isOnline
		logv("fireEvent", s"online: $isOnline,  listeners size: ${listeners.size }")
		listeners.foreach { s => TryNLog { s.onlineStatusChanged(isOnline) } }
		if (!_online) postCheck(LONG_SPAN)
		else postCheckCancel()
	}
}
