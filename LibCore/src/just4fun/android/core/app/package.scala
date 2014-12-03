package just4fun.android.core

package object app {


	/* STATES */
	object ActivityState extends Enumeration {
		val NONE, CREATED, STARTED, RESUMED, PAUSED, STOPPED, DESTROYED = Value
	}

	object ServicePhase extends Enumeration {
		val INIT, INITED, START, ACTIVE, STOP, STOPPED, FINALIZED = Value
	}

	object OperationStatus extends Enumeration {
		val OK, STOPPING, TIMEDOUT, FAILED = Value
	}



	/** TODO SYNCHRONOUS RESULT
	sealed abstract class Result[+A] {
		def isEmptyOrNull: Boolean
		def get: A
		final def getOrElse[B >: A](default: => B): B = if (isEmptyOrNull) default else this.get
		final def ifDefined[U](f: A => U): Unit = if (!isEmptyOrNull) f(this.get)
	}

	case class ResultOK[+A](x: A) extends Result[A] {
		def isEmptyOrNull = x == null
		def get: A = x
	}

	class ResultVal extends Result[Nothing] {
		def isEmptyOrNull = true
		def get = throw UnavailableException
	}

	case object Unavailable extends ResultVal
	case object Failed extends ResultVal
*/



	/* EXCEPTIONS */
	case class ServiceNotActiveException(serviceId: String, serviceState: String) extends Exception {
		override def getMessage: String = s"Service: [$serviceId], state: $serviceState.  ${super.getMessage}"
	}
	case class DependencyException(parentId: String, childId: String) extends Exception {
		override def getMessage: String = s"Parent: [$parentId], Child: [$childId].  ${super.getMessage}"
	}
	case class CyclicDependencyException(parentId: String, childId: String) extends Exception {
		override def getMessage: String = s"Parent: [$parentId], Child: [$childId].  ${super.getMessage}"
	}
	case object NoConfigException extends Exception
	case object TimeoutException extends Exception

}
