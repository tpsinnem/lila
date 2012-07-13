package lila
package memo

import akka.actor._
import akka.dispatch.{ Future, Await }
import akka.pattern.ask
import akka.util.Duration
import akka.util.duration._
import akka.util.Timeout
import play.api.Play.current
import play.api.libs.concurrent._
import com.google.common.cache.LoadingCache
import scala.collection.mutable

// all durations are expressed in seconds
final class MemCache {

  import MemCache._

  def apply(key: String, generate: ⇒ String, ttl: Int): String =
    apply(key, generate, ttl.some)

  def apply(key: String, generate: ⇒ String, ttl: Option[Int] = None): String = {
    hits get key filter (_ valid ttl)
  } | {
    Await.result(
      actor ? Request(() => generate) mapTo hitManifest, timeout
    ) ~ { hit ⇒
        hits += (key -> hit)
      }
  }

  private val hits = mutable.Map[String, Hit]()

  private val timeout = 1 minute
  private implicit val timeoutT = Timeout(timeout)

  private val actor = Akka.system.actorOf(Props(new Actor {
    def receive = {
      case Request(generate) ⇒ sender ! generate()
    }
  }))
}

object MemCache {

  case class Request(generate: () => String)

  case class Hit(val value: String, ts: Int) {

    def valid(ttl: Option[Int]) = ttl.fold(t ⇒ (ts + t) >= nowSeconds, true)
  }

  val hitManifest = manifest[Hit]
}
