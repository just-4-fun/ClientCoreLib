package just4fun.android.core.inet

import java.io.{ByteArrayOutputStream, BufferedInputStream, InputStreamReader, BufferedReader, EOFException, IOException, InputStream}
import scala.util.{Success, Failure, Try}
import java.net.{SocketTimeoutException, UnknownHostException, NoRouteToHostException, URLConnection, URLEncoder, URL, HttpURLConnection}
import scala.collection.mutable.Map
import scala.Some
import org.apache.http.NoHttpResponseException
import javax.net.ssl.SSLException
import just4fun.android.core.utils._
import just4fun.android.core.utils.time._
import just4fun.android.core.app.App
import project.config.logging.Logger._

case class InetRequest[T](opts: InetOptions, consumer: InputStream => Try[T], canceled: () => Boolean) extends Loggable {
import InetRequest._
	private[this] val startTime = now
	private[this] var attempt = 0
	private[this] var httpCode = 0
	private[this] var httpMessage: String = _
	private[this] var conn: HttpURLConnection = _

	def execute(): Try[T] = {
		var result: Try[T] = null
		do result = tryExec() while (result.isFailure && needRetry(result))
		result
	}

	private[this] def tryExec(): Try[T] = {
		try {
			if (canceled()) throw InetRequestCancelled
			if (!InetService.online) throw OfflineException
			attempt += 1
			prepare()
			logi("tryExec", startMessage)
			request() match {
				case result@Success(value) => logi("tryExec", resultMessage(value)); result
				case Failure(ex) => throw ex
			}
		}
		catch {case ex: Throwable => onError(ex); logw("tryExec", errorMessage(ex)); Failure(ex) }
		finally finalise()
	}
	private[this] def prepare() {
		if (opts.authenticator != null) opts.authenticator.onPrepareRequest(opts)
		// URL
		opts.fullUrl = opts.url
		if (nonEmpty(opts.urlArgs)) opts.fullUrl += "?" + argsToString(opts.urlArgs)
		val urlObject: URL = new URL(opts.fullUrl)
		// CONNECTION
		conn = urlObject.openConnection.asInstanceOf[HttpURLConnection]
		conn.setRequestMethod(opts.method.toString)
		conn.setConnectTimeout(opts.connectTimeoutMs)
		conn.setReadTimeout(opts.readTimeoutMs)
		conn.setInstanceFollowRedirects(opts.followRedirects)
		conn.setRequestProperty("Accept-Charset", UTF8)
		conn.setRequestProperty("Content-Type", opts.contentType)
		//conn.setRequestProperty("User-Agent", "?")
		//if (opts.authenticator != null) conn.setRequestProperty(opts.authenticator.getHeaderName(), opts.authenticator.getHeaderValue());
		setHeaders(conn)
		// PAYLOAD
		if (nonEmpty(opts.payloadArgs)) opts.payload(argsToString(opts.payloadArgs))
		if (opts.method != Method.GET && opts.payload != null) {
			conn.setDoOutput(true)
			conn.setFixedLengthStreamingMode(opts.payload.length)
			conn.getOutputStream.write(opts.payload)
		}
	}
	private[this] def argsToString(args: Map[String, Any]): String = {
		val body: StringBuilder = new StringBuilder
		args.foreach { case (k, v) =>
			body ++= s"${if (body.length > 0) "&" else "" }$k=${URLEncoder.encode(String.valueOf(v), UTF8) }"
		}
		body.toString()
	}
	private[this] def setHeaders(conn: URLConnection) = if (opts.headers != null && opts.headers.nonEmpty) {
		opts.headers.foreach { case (k, v) => conn.addRequestProperty(k, v.toString) }
	}
	private[this] def request(): Try[T] = {
		conn.connect()
		httpCode = conn.getResponseCode
		if (httpCode >= 200 && httpCode < 300) consumer(conn.getInputStream)
		else Failure(new IOException)
	}
	private[this] def wasteStream(in: InputStream) {
		TryNClose(in) { in =>
			val buf: Array[Byte] = new Array[Byte](1024)
			while (in.read(buf) != -1) {}
		}
	}
	private[this] def onError(ex: Throwable) = ex match {
		case ex: IOException => if (conn != null) TryNLog {
			httpMessage = conn.getResponseMessage
			val errStream = conn.getErrorStream
			val errInfo = if (errStream != null) (new StreamToString)(errStream).getOrElse("") else "ErrorStream = null"
			httpMessage = (if (httpMessage == null) "" else httpMessage + ";  ") + errInfo
		}
		case ex: InetRequestException => httpCode = ex.code
		case _ =>
	}
	private[this] def finalise() = if (conn != null) {
		//TryLog { wasteStream(conn.getInputStream)} // TODO ?
		Try { conn.getInputStream.close() }
		Try { conn.getOutputStream.close() }
		Try { conn.disconnect() }
		httpCode = 0
		httpMessage = null
	}
	private[this] def startMessage = s"STARTED...  attempt= $attempt;  method= ${opts.method };   url= ${opts.fullUrl };   payload len= ${if (opts.payload == null) 0 else opts.payload.length }"
	private[this] def errorMessage(e: Throwable) = s"FAILED  ::  httpCode= $httpCode;  Duration= ${now - startTime };  err= ${e.getClass.getSimpleName }:: ${e.getMessage };   httpMessage= $httpMessage"
	private[this] def resultMessage(result: Any) = s"OK  ::  ${
		result match {
			case r1: String => r1.take(200) + (if (r1.length > 200) "..." else "")
			case r2: Array[_] => r2.length + " length"
			case r3 => r3.getClass.getSimpleName + ".class"
		}
	}"
	private[this] def needRetry(result: Try[_]): Boolean = {
		val Failure(ex) = result
		var retry: Option[Boolean] = None
		//
		if (attempt > opts.maxAttempts || (opts.maxDuration > 0 && now - startTime > opts.maxDuration))
			retry = Some(false)
		else {
			if (opts.authenticator != null) retry = opts.authenticator.checkRetry(httpCode)
			if (retry == None && opts.errorHandler != null)
				retry = opts.errorHandler.handleErrorForRetry(httpCode, httpMessage, ex, opts)
			if (retry == None) retry = ex match {
				case _: NoHttpResponseException | _: NoRouteToHostException | _: UnknownHostException |
				  _: SocketTimeoutException | _: SSLException | _: EOFException => Some(true)
				case _ => Some(false)
			}
		}
		val value = retry.getOrElse(false)
		if (value) Try { Thread.sleep(500) }
		value
	}
}

