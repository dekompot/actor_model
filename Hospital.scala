package StudentsProduction

import StudentsProduction.Barn.{AskForAvailableSlot,Produce, ProduceOneInstance}
import akka.actor.typed._
import akka.actor.typed.scaladsl.Behaviors

import scala.util.Random


object Hospital {
  private final val SLOTS_NUMBER = 3
  private final val TIME_IN_MILISEC = 23328000f
  private final val SUCCESS_PROBABILITY = 0.85
  private var slotsTaken = 0
  private var finished = false

  def apply(next: ActorRef[Produce]): Behavior[Produce] = Behaviors.receive
  { (context, message) =>
    message match {
      case ProduceOneInstance(barn) =>
        slotsTaken = slotsTaken + 1
        Thread sleep (TIME_IN_MILISEC / Barn.getSpeed).toLong
        if ((Random.nextFloat()) <= SUCCESS_PROBABILITY)
          barn ! Barn.AddBaby(next)
        context.log.info("hospital asks barn for resources")
        slotsTaken = slotsTaken - 1
        barn ! Barn.AskForBabyResources(context.self)
        Behaviors.same
      case AskForAvailableSlot(barn) =>
        if (slotsTaken < SLOTS_NUMBER)
          barn ! Barn.AskForBabyResources(context.self)
        Behaviors.same
    }
  }
}
