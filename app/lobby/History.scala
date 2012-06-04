package lila
package lobby

import scala.math.max
import play.api.libs.json._
import scalaz.effects._
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.Imports._

final class History(collection: MongoCollection) {

  private var privateVersion: Int = fetchVersion.unsafePerformIO

  def version = privateVersion

  def since(v: Int): List[JsObject] = {
    (v < version).fold(
      fetchEvents(v + 1 -> version),
      Nil)
  }

  def +=(msg: JsObject): JsObject = {
    privateVersion = privateVersion + 1
    val v = version
    collection += DBObject("_id" -> version, "e" -> msg.toString)
    val vmsg = msg ++ JsObject(Seq("v" -> JsNumber(v)))
    vmsg
  }

  def set(key: String, value: Any): Unit = {
    collection += DBObject("_id" -> key, field -> value)
  }

  private def fetchVersion: IO[Int] = io {
    val result = collection
      .find(DBObject())
      .sort(DBObject("_id" -> -1))
      .limit(1)
    (for {
      row ← result.hasNext option result.next
      version ← row.getAs[Int]("version")
    } yield version) | 0
  }

  private def fetch: IO[Set[String]] = io {
    collection.find() map { obj ⇒
      obj.getAs[String]("_id")
    } 
  } map (_.flatten.toSet)
}
