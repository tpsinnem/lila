package lila
package game

import akka.actor._
import akka.util.duration._
import akka.util.{ Duration, Timeout }
import scala.collection.mutable
import org.joda.time.LocalDate

final class DayCount(gameRepo: GameRepo) extends Actor {

  def receive = {

    case GetNbGamesPerDay(days: Int) ⇒ sender ! {
      (hits get days) | {
        Hit(gameRepo.nbPerDay(days).unsafePerformIO, nowSeconds) ~ { hit ⇒
          hits += (days -> hit)
        }
      }
    }
  }

  private val ttl = 3600 * 6

  private val nbGames = mutable.Map[Int, Hit]()

  private def validate(hit: Hit) = 
    hit.timestamp > (nowSeconds - 

  private case class Hit(value: List[Int], timestamp: Int)
}
