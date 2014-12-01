package just4fun.android.core

import just4fun.android.core.app.App
import android.content.{SharedPreferences => Cache}
import android.content.SharedPreferences.Editor
import scala.util.Try

package object persist {

	/*  IMPLICITS */
	implicit def var2value[T](v: Var[T]): T = v.get()
	implicit def varOrdering[T](implicit ev: Ordering[T]): Ordering[Var[T]] = Ordering.by(v => v.value)


	/*  DEFAULTS */
	lazy val defaultCache = App().getSharedPreferences("cache", 0)


	/* BASE CLASS */
	abstract class Var[T](implicit cache: Cache) {
		var name: String
		var initial: T
		var temp: Boolean
		var value: T = if (temp) default else load()

		protected def fromString(s: String): T
		protected def default: T

		/** use implicit var2value as shortcut of call */
		def get(deep: Boolean = false): T = { if (deep) value = load(); value }
		def ~(v: T) = set(v)
		def set(v: T, deep: Boolean = true) = if (value != v) { value = v; if (deep) save(v, cache.edit).commit }
		def clear(deep: Boolean = true) { value = default; if (deep) cache.edit.remove(name).commit }
		protected def load(): T = Try { fromString(cache.getString(name, initial.toString)) }.getOrElse(initial)
		protected def save(v: T, editor: Editor): Editor = editor.putString(name, if (v == null) default.toString else v.toString)
		override def toString: String = if (value == null) "null" else value.toString
		override def equals(that: Any): Boolean = that match {
			case v: T => v == value
			case _ => super.equals(that)
		}

	}


	/* IMPLEMENTATIONS */

	case class StringVar(var name: String, var initial: String = "", var temp: Boolean = false)(implicit cache: Cache = defaultCache) extends Var[String] {
		override protected def fromString(s: String): String = s
		override protected def default: String = ""
	}

	case class LongVar(var name: String, var initial: Long = 0, var temp: Boolean = false)(implicit cache: Cache = defaultCache) extends Var[Long] {
		protected def fromString(s: String): Long = s.toLong
		override protected def default: Long = 0
	}

	case class IntVar(var name: String, var initial: Int = 0, var temp: Boolean = false)(implicit cache: Cache = defaultCache) extends Var[Int] {
		override protected def fromString(s: String): Int = s.toInt
		override protected def default: Int = 0
	}

	case class DoubleVar(var name: String, var initial: Double = 0, var temp: Boolean = false)(implicit cache: Cache = defaultCache) extends Var[Double] {
		override protected def fromString(s: String): Double = s.toDouble
		override protected def default: Double = 0
	}

	case class FloatVar(var name: String, var initial: Float = 0, var temp: Boolean = false)(implicit cache: Cache = defaultCache) extends Var[Float] {
		override protected def fromString(s: String): Float = s.toFloat
		override protected def default: Float = 0
	}

	case class BoolVar(var name: String, var initial: Boolean = false, var temp: Boolean = false)(implicit cache: Cache = defaultCache) extends Var[Boolean] {
		override protected def fromString(s: String): Boolean = s.toBoolean
		override protected def default: Boolean = false
	}


}
