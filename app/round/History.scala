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

final class History(
    collection: MongoCollection,
    gameId: String) {

  def version = privateVersion

  def since(v: Int): List[VersionedEvent] = (v < version).fold(
    fetchEvents(v),
    Nil)

  def +=(events: List[Event]): List[VersionedEvent] = {
    val newVersion = version + events.size
    val eventsWithVersions = events zip (version to newVersion)
    privateVersion = newVersion
    val eventObjects = eventsWithVersions map {
      case (event, v) ⇒ DBObject(
        "v" -> v,
        "t" -> event.typ,
        "d" -> event.data.toString,
        "f" -> event.only.map(_.name),
        "o" -> event.owner)
    }
import com.mongodb.casbah.WriteConcern
    collection.update(
      idSelector,
      DBObject("$pushAll" -> DBObject("e" -> eventObjects)),
      upsert = true,
      multi = false,
      writeConcern = WriteConcern.Safe)

    eventsWithVersions map {
      case (event, v) ⇒
        versionedEvent(v, event.typ, event.data, event.only, event.owner)
    }
  }

  private def versionedEvent(v: Int, typ: String, data: JsValue, only: Option[Color], own: Boolean) =
    VersionedEvent(
      js = JsObject(Seq("v" -> JsNumber(v), "t" -> JsString(typ), "d" -> data)),
      only = only,
      own = own)

  private def fetchEvents(from: Int): List[VersionedEvent] = {
    for {
      row ← collection.findOne(idSelector, "e" $slice (version - from))
      events ← row.getAs[BasicDBList]("e")
      eventList = new MongoDBList { val underlying = events }
    } yield (eventList map extractEvent).toList.flatten
  } | Nil

  private def extractEvent(event: Any): Option[VersionedEvent] = {
    val obj = wrapDBObj(event.asInstanceOf[DBObject])
    for {
      v ← obj.getAs[Int]("v")
      t ← obj.getAs[String]("t")
      d ← obj.getAs[String]("d")
      data = Json parse d
      f = obj.getAs[String]("f")
      only = f flatMap Color.apply
      o ← obj.getAs[Boolean]("o")
    } yield versionedEvent(v, t, data, only, o)
  }
  //catch {
  //case e ⇒ {
  //println("Error when extracting event for %s - %s".format(gameId, e.getMessage))
  //None
  //}
  //}

  private var privateVersion: Int = {
    for {
      row ← collection.findOne(idSelector, DBObject("e.v" -> true))
      events ← row.getAs[BasicDBList]("e")
    } yield events.size
  } | 0

  private def idSelector = DBObject("_id" -> gameId)
}

object History {

  def apply(collection: MongoCollection)(gameId: String): History =
    new History(collection, gameId)
}
