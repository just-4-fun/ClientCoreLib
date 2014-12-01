package just4fun.android.core

import scala.collection.GenTraversableOnce
import android.os.SystemClock
import scala.util.{Failure, Success, Try}
import project.config.logging.Logger._

package object utils {

	def isEmpty(value: Any): Boolean = {
		print(value + "  ")
		value match {
			case null => true
			case v: String => v.isEmpty
			case v: Option[_] => v.isEmpty || isEmpty(v.get)
			case v: GenTraversableOnce[_] => v.isEmpty
			case v: Int => v == 0
			case v: Long => v == 0
			case v: Double => v == 0
			case v: Float => v == 0
			case v: Short => v == 0
			case v: Byte => v == 0
			case v: Char => v == 0
			case v: Boolean => !v
			case v: Unit => true
			case _ => false
		}
	}
	def nonEmpty(value: Any): Boolean = !isEmpty(value)

	object time {
		def now = System.currentTimeMillis
		def deviceNow = SystemClock.elapsedRealtime
	}

	object TryNLog {
		def apply[T](codeBlock: => T)(implicit tag: LogTag): Try[T] = {
			Try { codeBlock } match {
				case f@Failure(e) => loge(e)(tag); f
				case res => res
			}
		}
	}
	object TryNClose {
		def apply[U <: {def close()}, R](r: U)(f: U => R): Try[R] = {
			try {Success(f(r))} catch {case e: Throwable => Failure(e) }
			finally {try {if (r != null) r.close()} catch {case _: Throwable => () } }
		}
	}

	def stackTrace(ex: Throwable): String = {
		// TODO
		ex.getStackTrace.toString
	}

	def printObject(obj : Any): String = "" // TODO
}
