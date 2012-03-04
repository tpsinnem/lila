package lila.system

import model._
import com.mongodb.casbah.Imports._

class GameMapperTest extends SystemTest {

  import GameMapper._

  "the game mapper" should {
    //"encode a game" in {
    //asDbObject(newDbGame) must_== MongoDBObject(
    //"id" -> "arstdhne"
    //)
    //}
    "decode and recode a game" in {
      asObject(asDbObject(newDbGame)) must_== Some(newDbGame)
    }
  }
}
