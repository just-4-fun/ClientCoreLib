package just4fun.android.core.sqlite

import android.content.ContentValues
import android.database.Cursor
import just4fun.android.core.app.{OperationStatus, ParallelThreadFeature, App, AppService}
import android.database.sqlite.{SQLiteException, SQLiteQueryBuilder, SQLiteDatabase, SQLiteOpenHelper}
import just4fun.android.core.async.Async._
import just4fun.android.core.async.FutureExt
import just4fun.android.core.utils._
import project.config.logging.Logger._
import OperationStatus._

import scala.util.{Failure, Try, Success}


/* DB SERVICE */
class DbService(val name: String = "main") extends Db with AppService with ParallelThreadFeature {

	override protected def onInitialize(): Unit = {
		val dbHelper = new SQLiteOpenHelper(App(), name, null, 1) {
			override def onCreate(db: SQLiteDatabase): Unit = {}
			override def onUpgrade(db: SQLiteDatabase, oldVersion: Int, newVersion: Int): Unit = {}
		}
		db = dbHelper.getWritableDatabase // SQLiteException if the database cannot be opened for writing
	}
	override protected def isStarted(operationIsOK: Boolean = operation == NORMAL) = db != null && db.isOpen
	override protected def onStop(operationIsOK: Boolean = operation <= FINISHING): Unit = asyncExecContext.quit(true)
	override protected def isStopped(operationIsOK: Boolean = operation <= FINISHING) = asyncExecContext.isQuit
	override protected def onFinalize(operationIsOK: Boolean = operation <= FINISHING): Unit = { if (isStarted()) db.close(); db = null }
}






/* DB */

class Db extends Loggable {
	self: DbService =>
	var db: SQLiteDatabase = _
	def dbExecContext = asyncExecContext
	def dbName = name

	/* ASYNC USAGE */

	/**
	  */
	def execSql(sql: String): FutureExt[Unit] = post("execSql", replace = false) {execSqlSync(sql)}
	def select(sql: String): FutureExt[Cursor] = post("select", replace = false) {selectSync(sql).get}
	/**
	@example {{{
		selectNClose("SELECT 1 FROM Tab"){cursor => cursor.getInt(0)} onComplete {
			case Success(v) => println(v)
			case Failure(ex) => println(ex)
		}
		}}}
	  */
	def selectNClose[T](sql: String)(code: Cursor => T): FutureExt[T] = post("selectNClose", replace = false) {
		selectNCloseSync(sql)(code).get
	}
	def insert(table: String, values: ContentValues): FutureExt[Long] =
		post("insert", replace = false) {insertSync(table, values).get}
	def update(table: String, values: ContentValues, where: String = null): FutureExt[Int] =
		post("update", replace = false) {updateSync(table, values).get}
	def delete(table: String, where: String = null): FutureExt[Int] =
		post("delete", replace = false) {deleteSync(table, where).get}

	/* SYNC USAGE */

	def execSqlSync(sql: String): Try[Unit] = ifActive {
		logv("execSql", sql)
		db.execSQL(sql)
	}
	/**
	  */
	def buildQuery(table: String, columns: Array[String] = null, where: String = null, groupBy: String = null, having: String = null, orderBy: String = null, limit: String = null, distinct: Boolean = false): String = {
		SQLiteQueryBuilder.buildQueryString(distinct, table, columns, where, groupBy, having, orderBy, limit)
	}
	/**
	  */
	def selectSync(sql: String): Try[Cursor] = {
		logv("select", sql)
		ifActive {db.rawQuery(sql, null)}
	}
	/**
	@example {{{
		val num: Int = selectNCloseSync("SELECT 1 FROM Tab"){cursor => cursor.getInt(0)} match {
			case Success(v) => v
			case Failure(_) => 0
		}
	 }}}
	  */
	def selectNCloseSync[T](sql: String)(code: Cursor => T): Try[T] = {
		selectSync(sql) match {
			case Success(cursor) => try {Success(code(cursor))}
			finally try {if (!cursor.isClosed) cursor.close()} catch {case _: Throwable =>}
			case Failure(ex) => Failure(ex)
		}
	}

	/**
	 * @return  [[Success]] of row ID of the newly inserted row, or [[Failure]] otherwise
	 */
	def insertSync(table: String, values: ContentValues): Try[Long] = ifActive {
		logv("insert", s"Tab: $table,  values: ${printObject(values)}")
		db.insert(table, null, values) match {
			case -1 => throw new SQLiteException("Insertion failed")
			case id => id
		}
	}
	/**
	  */
	def updateSync(table: String, values: ContentValues, where: String = null): Try[Int] = ifActive {
		logv("update", s"Tab: $table,  where: $where,  values: ${printObject(values)}")
		db.update(table, values, where, null)
	}
	/**
	 * @note To remove all rows and get a count pass "1" as the whereClause.
	 */
	def deleteSync(table: String, where: String = null): Try[Int] = ifActive {
		logv("delete", s"Tab: $table,  where: $where")
		db.delete(table, where, null)
	}

	def execInTransaction[T](code: => T): T = {
		try {
			db.beginTransaction()
			val res = code
			db.setTransactionSuccessful()
			res
		}
		finally if (db.inTransaction()) db.endTransaction()
	}


}