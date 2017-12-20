package wumpus

import Game._
import akka.actor._

import scala.util.Random

object Game {
  sealed trait Room

  case object Empty extends Room
  case object WumpusInside extends Room
  case object BottomLessPit extends Room
  case object ArrowInside extends Room

  sealed trait GameState
  case class GameRunning(arrows: Int, room: Room) extends GameState
  case object GameOver extends GameState

  def checkRoom(game: GameRunning): GameState = game.room match {
    case Empty => game
    case WumpusInside => GameOver
    case BottomLessPit => GameOver
    case ArrowInside => game.copy(arrows = game.arrows + 1, room = Empty)
    case _ => ???
  }
}

class WumpusGame(room: Room) extends Actor {
  def state(room: Room, arrows: Int = 5): GameRunning =
    GameRunning(arrows, room)

  def receive = {
    case _ => checkRoom(state(room))
  }
}

object StartGame extends App {
  val system = ActorSystem("WumpusActor")

  val rooms = Array(Empty, WumpusInside, BottomLessPit, ArrowInside)
  val wumpusActor = system.actorOf(Props(new WumpusGame(Random.shuffle(rooms.toList).head)), name = "WumpusActor")

  wumpusActor ! Empty
  wumpusActor ! WumpusInside
}