package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case object RetrySnap

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import context.dispatcher

  context.system.scheduler.schedule(100 millis, 100 millis, context.self, RetrySnap)

  var acks = Map.empty[Long, (ActorRef, Replicate)]
  var pending = Vector.empty[Snapshot]

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  def receive: Receive = {
    case rep @ Replicate(key, valueOpt, id) => {
      val seq = nextSeq
      acks += seq -> (sender, rep)
      replica ! Snapshot(key, valueOpt, seq)
    }
    case SnapshotAck(key, seq) => {
      acks.get(seq).map{entry =>
        val (primary, command) = entry
        primary ! Replicated(key, command.id)
      }
      acks -= seq
    }
    case RetrySnap => {
      acks.foreach(entry => {
        val (seq, (primary, replicate)) = entry
        replica ! Snapshot(replicate.key, replicate.valueOption, seq)
      })
    }
  }
}