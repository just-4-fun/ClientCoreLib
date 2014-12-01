package just4fun.android.core.inet
import collection.mutable.Map

case class InetOptions(url: String, var method: Method.Value = Method.GET, var contentType: String = ContentType.Form, var connectTimeoutMs: Int = 20000, var readTimeoutMs: Int = 65000, var maxAttempts: Int = 1, var maxDuration: Long = 0L, var followRedirects: Boolean = true) {
	// request params
	var fullUrl: String = _
	var headers: Map[String, Any] = _
	var urlArgs: Map[String, Any] = _
	var payloadArgs: Map[String, Any] = _
	var payload: Array[Byte] = _
	// optional params
	var authenticator: InetAuthenticator = _
	var errorHandler: ErrorHandler = _

	def payload(v: Seq[Byte]): InetOptions = { payload = v.toArray; this }
	def payload(v: String): InetOptions = { payload = v.getBytes; this }
	def addPayloadArg(name: String, nonEncodedV: Any): InetOptions = {
		if (payloadArgs == null) payloadArgs = Map.empty
		payloadArgs += (name -> nonEncodedV)
		this
	}
	def urlArgs(v: TraversableOnce[(String, Any)]): InetOptions = {
		if (urlArgs == null) urlArgs = Map.empty
		urlArgs ++= v
		this
	}
	def addUrlArg(name: String, nonEncodedV: Any): InetOptions = {
		if (urlArgs == null) urlArgs = Map.empty
		urlArgs += (name -> nonEncodedV)
		this
	}
	def headers(v: TraversableOnce[(String, Any)]): InetOptions = {
		if (headers == null) headers = Map.empty
		headers ++= v
		this
	}
	def addHeader(name: String, v: Any): InetOptions = {
		if (headers == null) headers = Map.empty
		headers += (name -> v)
		this
	}
	def authenticator(v: InetAuthenticator): InetOptions = { authenticator = v; this }
	def errorHandler(v: ErrorHandler): InetOptions = { errorHandler = v; this }
	override def clone: InetOptions = {
		val clone = new InetOptions(url)
		getClass.getDeclaredFields.foreach{f =>  f.setAccessible(true); f.set(clone, f.get(this))}
		clone
	}
}
