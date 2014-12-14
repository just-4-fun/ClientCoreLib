package just4fun.android.core.app

trait Initializer {
	def preInitialize(): Unit = ()
	def postUtilize(): Unit = ()
}

