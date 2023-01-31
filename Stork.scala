package StudentsProduction

import StudentsProduction.Barn.{AskForAvailableSlot, Produce, ProduceOneInstance}
import akka.actor.typed._
import akka.actor.typed.scaladsl.Behaviors

import scala.util.Random

object Stork {

  private final val SLOTS_NUMBER = 1
  private final val TIME_IN_MILISEC = 500f
  private final val SUCCESS_PROBABILITY = 0.99
  private var slotsTaken = 0

  def apply(next: ActorRef[Produce]): Behavior[Produce] = Behaviors.receive
  { (context, message) =>
    message match {
      case ProduceOneInstance(barn) =>
        slotsTaken = slotsTaken + 1
        Thread sleep ((TIME_IN_MILISEC + TIME_IN_MILISEC * Random.nextFloat) / Barn.getSpeed).toLong
        if ((Random.nextFloat()) <= SUCCESS_PROBABILITY)
          barn ! Barn.AddPreBaby(next)
        context.log.info("school asks barn for resources")
        slotsTaken = slotsTaken - 1
        barn ! Barn.AskForPreBabyResources(context.self)
        Behaviors.same
      case AskForAvailableSlot(barn) =>
        if (slotsTaken < SLOTS_NUMBER)
          barn ! Barn.AskForPreBabyResources(context.self)
        Behaviors.same
    }

  }

}
