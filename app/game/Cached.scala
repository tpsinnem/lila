package lila
package game

import akka.util.duration._

import memo.ActorMemo

final class Cached(
    gameRepo: GameRepo,
    nbTtl: Int) {

  import Cached._

  def nbGames: Int = fastMemo(NbGames)
  def nbMates: Int = fastMemo(NbMates)
  def nbPopular: Int = fastMemo(NbPopular)
  def yearGames: List[Int] = slowMemo(YearGames)

  private val fastMemo = ActorMemo(fastFromDb, nbTtl, 5.seconds)
  private def fastFromDb(key: FastKey) = key match {
    case NbGames ⇒ gameRepo.count(_.all).unsafePerformIO
    case NbMates ⇒ gameRepo.count(_.mate).unsafePerformIO
    case NbPopular ⇒ gameRepo.count(_.popular).unsafePerformIO
  }

  private val slowMemo = ActorMemo(slowFromDb, nbTtl, 5.seconds)
  private def slowFromDb(key: SlowKey) = key match {
    case YearGames ⇒ gameRepo.nbPerDay(365).unsafePerformIO
  }
}

object Cached {

  sealed trait FastKey
  case object NbGames extends FastKey
  case object NbMates extends FastKey
  case object NbPopular extends FastKey

  sealed trait SlowKey
  case object YearGames extends SlowKey
}
