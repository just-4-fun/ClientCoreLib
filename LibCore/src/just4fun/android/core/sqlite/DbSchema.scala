package just4fun.android.core.sqlite

import android.content.ContentValues
import android.database.Cursor
import just4fun.android.core.app.{OperationStatus, AppService}
import just4fun.android.core.async._
import just4fun.android.core.async.Async._
import just4fun.android.core.persist._
import just4fun.core.schema.formatters.JsonArrayFormatter
import just4fun.core.schema._
import just4fun.core.xpression.{SqlContext, XNode, BuildContext, PropXNode}
import project.config.logging.Logger._
import OperationStatus._

import scala.collection.{GenTraversableOnce => Coll, SortedSet}
import scala.util.{Failure, Success, Try}


/* TABLE SERVICE */

trait DbTableService extends AppService {
	self: DbSchema[_] =>

	/** All async requests will run in db allocated thread. */
	override implicit def asyncExecContext: AsyncExecContext = db.dbExecContext

	override protected def onStart(): Unit = {
		open().onComplete {
			case Success(_) => setStarted
			case Failure(e) => failure = e
		}
	}
	override protected def onStop(operationIsOK: Boolean = operation == OK): Unit = {
		close().onComplete {
			case Success(_) => setStopped
			case Failure(e) => failure = e
		}
	}
}








/* DB SCHEMA */

trait DbSchemaImpl[S <: DbSchemaImpl[S]] extends Schema[S] {
	self: S =>
	override type OBJ = DbObj
	override type PROP[T] = DbProp[T]
	override implicit val utils = new JsonArrayFormatter with TypeUtils {}

	override protected def newObject: OBJ = new DbObj
	override protected def newProp[T: Manifest](index: Byte, name: String, valueType: PropType[T] with BaseType[_]): PROP[T] = DbProp[T](index, name, valueType)


	/* ID Prop */
	val _id = Lng_prop()("_id", {prop => prop.constraint = "PRIMARY KEY AUTOINCREMENT";  prop.forceName = true})



	/* DB OBJ */
	class DbObj extends Obj {
		def table = scheme
	}



	/* DB PROP */
	case class DbProp[T: Manifest](index: Byte, name: String, valueType: PropType[T] with BaseType[_]) extends Prop[T] with PropXNode {
		var constraint: String = _

		nodeType = {
			val tp = implicitly[Manifest[T]].toString()
			val ix = tp.lastIndexOf('.')
			(if (ix < 0) tp else tp.substring(ix + 1)).toLowerCase
		}

		override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
			builder ++= (if (cxt.indexBasedName) indexName else name)
		}

		def sqlType = valueType match {
			case _: DoubleBaseType => "FLOAT"
			case _: NumberBaseType[_] => "INTEGER"
			case _: StringBaseType => "STRING"
			case _: BinaryBaseType => "BLOB"
			case _ => "STRING"
		}
		def sqlCreate = s"$indexName $sqlType${if (constraint == null) "" else " " + constraint}"
	}
}









/* TABLE */

object DbSchema {
	implicit def xnode2string(expr: XNode): String = expr.toExpression(SqlContext.SQLITE)
}


abstract class DbSchema[S <: DbSchema[S]](val db: Db, override val theName: String) extends DbSchemaImpl[S] with DbTableService {
	self: S =>
	import DbSchema._
	var upgradeVersionCount = 0

	/* OVERRIDE */
	protected def indexes: Set[DbTableIndex] = Set.empty
	protected def upgrades: Vector[DbTableUpgrade] = Vector.empty

	//SYNCHRONOUS TABLE LIFECYCLE CALLBACKS

	/** WARN! All database operations inside method should be performed synchronously i.e. in current thread. */
	protected def onTableCreate(): Unit = ()
	/** WARN! All database operations inside method should be performed synchronously i.e. in current thread. */
	protected def onTableOpen(): Unit = ()
	/** WARN! All database operations inside method should be performed synchronously i.e. in current thread. */
	protected def onCloseTable(): Unit = ()

	/* PUBLIC  API */

	def select(condition: String = null, dbProps: Set[S#DbProp[_]] = null, groupBy: String = null, having: String = null, orderBy: String = null, limit: String = null, distinct: Boolean = false): FutureExt[Seq[S#OBJ]] = post(s"select $theName", replace = false) {
		selectSync(condition, dbProps, groupBy, having, orderBy, limit, distinct).get
	}
	def selectSync(condition: String = null, dbProps: Set[S#DbProp[_]] = null, groupBy: String = null, having: String = null, orderBy: String = null, limit: String = null, distinct: Boolean = false): Try[Seq[S#OBJ]] = {
		// EXEC
		//
		val _props = if (dbProps == null) props else dbProps + _id
		val cols = _props.map(_.indexName).toArray
		val q = db.buildQuery(theName, cols, condition, groupBy, having, orderBy, limit, distinct)
		//
		// DEFs
		//
		def prev(list: List[S#OBJ], cursor: Cursor, ok: Boolean): List[S#OBJ] = {
			if (ok) {
				val obj = this().setValuesOf(_props) { (ix, prop) =>
					cursor.getType(ix) match {
						case Cursor.FIELD_TYPE_STRING => cursor.getString(ix)
						case Cursor.FIELD_TYPE_INTEGER => cursor.getLong(ix)
						case Cursor.FIELD_TYPE_FLOAT => cursor.getDouble(ix)
						case Cursor.FIELD_TYPE_BLOB => cursor.getBlob(ix)
						case _ => null
					}
				}
				prev(obj :: list, cursor, cursor.moveToPrevious())
			}
			else list
		}
		// EXEC
		//
		db.selectNCloseSync(q) { cursor => prev(List[S#OBJ](), cursor, cursor.moveToLast())}
	}
	/** Async version of saveSync. @see saveSync */
	def save(objects: S#OBJ*): FutureExt[Unit] = post(s"save $theName", replace = false) {
		saveSync(objects: _*).get
	}
	/** Inserts or updates object in db depending whether _id is set or not.
	@return status of whole operation. Each object ''changed'' indicates success if false. Inserted object _id is set to actual value.  */
	def saveSync(objects: S#OBJ*): Try[Unit] = {
		// DEFs
		//
		def values(obj: S#OBJ, cols: Iterable[S#DbProp[_]]): ContentValues = {
			import SchemaImplicits._
			val values = new ContentValues
			obj.getValuesOf(cols) { (ix, prop, v) =>
				prop match {
					case c: StringBaseType => values.put(c.indexName, c.asBaseType(v))
					case c: LongBaseType => values.put(c.indexName, c.asBaseType(v): java.lang.Long)
					case c: DoubleBaseType => values.put(c.indexName, c.asBaseType(v))
					case c: BooleanBaseType => values.put(c.indexName, c.asBaseType(v))
					case c: BinaryBaseType => values.put(c.indexName, c.asBaseType(v))
					case _ => values.put(prop.indexName, prop.asString(v))
				}
			}
			values
		}
		// EXEC
		//
		var error: Throwable = null
		objects.map { obj =>
			if (obj.hasValue(_id)) db.updateSync(theName, values(obj, obj.propsChanged), _id === obj(_id)) match {
				case Success(_) => obj.clearChanged()
				case Failure(err) => error = err
			}
			else db.insertSync(theName, values(obj, props)) match {
				case Success(id) => obj.setRawValue(_id, id); obj.clearChanged()
				case Failure(err) => error = err
			}
		}
		if (error == null) Success() else Failure(error)
	}
	def delete(objects: S#OBJ*): FutureExt[Int] = post(s"delete $theName", replace = false) {
		deleteSync(objects: _*).get
	}
	def deleteSync(objects: S#OBJ*): Try[Int] = {
		val condition = _id.in(objects.map(o => o(_id)): _*)
		db.deleteSync(theName, condition)
	}


	/* INTERNAL API */

	def fullName: String = db.dbName + "_" + theName

	def open(): FutureExt[Unit] = {
		val version = IntVar(s"${fullName}_version", -1)
		val _indexes = indexes
		val _upgrades = upgrades
		//
		// DEFs
		//
		def updateSchema(): Unit = {
			// Drop excessive indexes
			val oldIndexes = loadIndexes()
			dropIndexes(oldIndexes diff _indexes)
			// Add new columns
			val columnTypes = loadColumnTypes()
			propsAll.drop(columnTypes.length).foreach(addColumn)
			// Execute upgrades
			applyUpgrades()
			// check if recreate table (if column types changed)
			val recreate = propsAll.zip(columnTypes).forall { case (prop, typ) => prop.sqlType == typ}
			// Recreate whole table or just create missing Indexes
			if (recreate) recreateTable(columnTypes.length)
			else createIndexes(_indexes diff oldIndexes)
		}
		def loadIndexes(): Set[DbTableIndex] = {
			def next(indexes: Set[DbTableIndex], cursor: Cursor, nameIx: Int): Set[DbTableIndex] =
				if (cursor.moveToNext) next(indexes + new DbTableIndex(cursor.getString(nameIx)), cursor, nameIx)
				else indexes
			//
			db.selectNCloseSync(s"PRAGMA index_list($theName)") { cursor =>
				val nameIx = cursor.getColumnIndex("name")
				next(Set[DbTableIndex](), cursor, nameIx)
			}.get
		}
		def dropIndexes(indexes: Coll[DbTableIndex]) = indexes.foreach { index =>
			execQuery(s"DROP INDEX IF EXISTS ${index.name}")
		}
		def createIndexes(indexes: Coll[DbTableIndex]) = indexes.foreach { index =>
			execQuery(s"CREATE INDEX IF NOT EXISTS ${index.name} ON $theName (${index.dbPropNames}})")
		}
		def loadColumnTypes(): Seq[String] = {
			def next(cols: Vector[String], cursor: Cursor, typeIx: Int): Vector[String] =
				if (cursor.moveToNext) next(cols :+ cursor.getString(typeIx).toUpperCase, cursor, typeIx)
				else cols
			//
			db.selectNCloseSync(s"PRAGMA table_info($theName)") { cursor =>
				val typeIx = cursor.getColumnIndex("type")
				next(Vector[String](), cursor, typeIx)
			}.get
		}
		def addColumn(prop: S#DbProp[_]) = execQuery(s"ALTER TABLE $theName ADD COLUMN ${prop.sqlCreate}")
		def applyUpgrades() = _upgrades.dropWhile(_.version <= version).foreach { upgrade =>
			upgrade.prop_eq_expressions.foreach { case (prop, expr) =>
				execQuery(s"UPDATE $theName SET ${prop.indexName}=${expr.toExpression(SqlContext.SQLITE)}")
			}
		}
		def createTable(): Unit = {
			val cols = propsAll.map(_.sqlCreate).mkString(",")
			execQuery(s"CREATE TABLE IF NOT EXISTS $theName ($cols)", false)
		}
		def recreateTable(columnLength: Int) = {
			// rename old table
			val oldTable = "_" + theName
			execQuery(s"ALTER TABLE $theName RENAME TO $oldTable")
			// create new table
			createTable()
			// copy old values to new table
			val colStr = propsAll.take(columnLength).map(_.indexName).mkString(",")
			execQuery(s"INSERT INTO $theName ($colStr) SELECT $colStr FROM  $oldTable")
			// drop old table
			execQuery(s"DROP TABLE IF EXISTS $oldTable")
			// create indexes
			createIndexes(_indexes)
		}
		def execQuery(q: String, silent: Boolean = true) = db.execSqlSync(q) match {
			case Failure(e) => if (silent) loge(e) else throw e
			case _ =>
		}
		//
		// EXECUTION
		//
		post(s"open $fullName") {
			// TODO implicit var2value does not work ??? version == -1
			if (version.get() == -1) {
				createTable()
				createIndexes(_indexes)
				onTableCreate()
			}
			else updateSchema()
			version ~ upgradeVersionCount
			onTableOpen()
		}
	}

	def close(): FutureExt[Unit] = post(s"close $fullName") {onCloseTable()}




	/* HELPER CLASSES */

	case class DbTableIndex(dbProps: DbProp[_]*) {
		def this(_name: String) = { this(); name = _name }
		var name = dbProps.foldLeft("")((res, prop) => res + prop.indexName)
		def dbPropNames = dbProps.map(_.indexName).mkString(",")
		override def equals(other: Any): Boolean = other match {
			case ix: DbTableIndex => name == ix.name
			case _ => false
		}
	}

	/**
	@param prop_eq_expressions (dbProp, expression) where: column = expression
	  */
	case class DbTableUpgrade(prop_eq_expressions: (DbProp[_], XNode)*) {
		upgradeVersionCount += 1
		val version = upgradeVersionCount
	}

}
