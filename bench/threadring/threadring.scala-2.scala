/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Stefan Ettrup
*/
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props

class ThreadRingActor(id : Int) extends Actor {
  var next: ActorRef = null

  def receive: Actor.Receive = {
    case e: Integer => {
      if (e == 0) {
        println(id); System.exit(0)
      } else
        next.tell(e - 1, null);
    }
    case n: ActorRef => { this.next = n }
  }
}

object threadring {
  val ring = 503

  def main(args: Array[String]) {
    val system = ActorSystem()

    val actors = (for (i <- 1 to ring)
      yield system.actorOf(Props(classOf[ThreadRingActor],i), (i).toString())).toArray

    for (i <- 0 until ring) {
      if (i == ring - 1) actors(i) ! actors(0)
      else actors(i) ! actors(i + 1)
    }

    actors(0) ! args(0).toInt
  }
}
