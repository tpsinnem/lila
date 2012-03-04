package lila.system

import model._

import com.mongodb.casbah.Imports._

object GameMapper {

  def asObject(dbo: DbObject): Option[DbGame] = asObject {
    new MongoDBObject { val underlying = dbo }
  }

  def asObject(dbo: MongoDBObject): Option[DbGame] = {
    for {
      id ← dbo.getAs[String]("id")
      players = Nil
      pgn ← dbo.getAs[String]("pgn")
      status ← dbo.getAs[Int]("status")
      turns ← dbo.getAs[Int]("turns")
      clock = dbo.getAs[Map[String, Any]]
      lastMove = None
      positionHashes ← dbo.getAs[String]("positionHashes")
      castles ← dbo.getAs[String]("castles")
      isRated ← dbo.getAs[Boolean]("isRated")
    } yield DbGame(
      id = id,
      players = players,
      pgn = pgn,
      status = status,
      turns = turns,
      clock = clock,
      lastMove = lastMove,
      positionHashes = positionHashes,
      castles = castles,
      isRated = isRated)
  }

  def asDbObject(o: DbGame): DBObject = MongoDBObject(
    "id" -> o.id,
    "pgn" -> o.pgn,
    "status" -> o.status,
    "turns" -> o.turns,
    "lastMove" -> (o.lastMove getOrElse null),
    "positionHashes" -> o.positionHashes,
    "castles" -> o.castles,
    "isRated" -> o.isRated
  )
}
