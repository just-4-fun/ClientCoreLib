package just4fun.android.core.app

import java.lang.Thread.UncaughtExceptionHandler

import android.app.Application
import android.content.Context
import android.content.pm.PackageManager
import project.config.logging.Logger._


/* GLOBAL ACCESSOR */

object App extends AppSingleton[App]


abstract class AppSingleton[T <: App] {
	// WARN: throws error if T is not an  Application subclass declared in Manifest.xml.
	private lazy val app: T = just4fun.android.core.app.appInstance.asInstanceOf[T]
//	def apply(): T = app
	def context: Context = app
	def control: AppController = app
	def config: AppConfig = app
	def utils: AppUtils = app
}




/* APP class */

abstract class App extends Application with Loggable with AppController with AppConfig with AppUtils /*TODO with Inet with Db etc*/ {

	/* UNCAUGHT EXCEPTIONS */
	Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler {
		val defaultHandler = Thread.getDefaultUncaughtExceptionHandler
		override def uncaughtException(thread: Thread, ex: Throwable): Unit = {
			loge(ex, s"Uncaught Exception.")
			defaultHandler.uncaughtException(thread, ex)
		}
	})


	just4fun.android.core.app.appInstance = this
	// set thread pool [[async.NewThreadContext]]
	just4fun.android.core.async.threadPoolExecutor = threadPoolExecutor

	override protected def onCreate(): Unit = {
		initialize()
		logw("", s"${" " * (60 - 3) }   >   APP  CREATED  #${hashCode() };  WAS CRASHED ? ${CrashMarker.crashFlag}")
	}


}
