package lila
package round

import play.api.libs.json._
import scalaz.effects._
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.Imports._

import chess.Color
import memo.Builder

case class VersionedEvent(js: JsObject, only: Option[Color], own: Boolean) {

  def visible(m: Member): Boolean =
    if (own && !m.owner) false else only.fold(_ == m.color, true)
}

// db.game_history.ensureIndex({g:1})
// db.game_history.ensureIndex({v:-1})
final class History(
    collection: MongoCollection,
    gameId: String) {

  private var privateVersion = fetchVersion

  def version = privateVersion

  def since(v: Int): List[VersionedEvent] = (v < version).fold(
    fetchEvents(v),
    Nil)

  def +=(event: Event): JsObject = {
    privateVersion = privateVersion + 1
    val v = version
    collection.insert(DBObject(
      "_id" -> gameId + v,
      "g" -> gameId,
      "v" -> v,
      "t" -> event.typ,
      "d" -> event.data.toString,
      "f" -> event.only,
      "o" -> event.owner))

    versionedEvent(v, event.typ, event.data, event.only, event.owner)
  }

  private def versionedEvent(v: Int, typ: String, data: JsObject, only: Option[Color], owner: Boolean) =
    VersionedEvent(
      js = JsObject(Seq("v" -> JsNumber(v), "t" -> JsString(typ), "d" -> data)),
      only = only,
      owner = owner)

  private def fetchEvents(from: Int): List[VersionedEvent] = for {
    row ← collection.findOne(idSelector, "e" $slice (version - from))
    events ← row.getAs[BasicDBList]("e")
    eventList = new MongoDBList { val underlying = events }
  } yield (eventList map extractEvent).flatten

  private def extractEvent(event: Any): Option[VersionedEvent] = try {
    val obj = event.asInstanceOf[MongoDBObject]
    for {
      v ← obj.getAs[Int]("v")
      t ← obj.getAs[String]("t")
      d ← obj.getAs[String]("d")
      data ← Json parse d
      f = obj.getAs[String]("f")
      only = f flatMap Color.apply
      o ← obj.getAs[Boolean]("o")
    } yield versionedEvent(v, t, data, only, o)
  }
  catch {
    case e ⇒ {
      println("Error when extracting event for %s - %s".format(gameId, e.getMessage))
      None
    }
  }

  private def fetchVersion: Int = 
    collection.count(DBObject("g" -> gameId))
}

object History {

  def apply(collection: MongoCollection)(gameId: String): History =
    new History(collection, gameId)
}
