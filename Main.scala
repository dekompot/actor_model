package StudentsProduction

import akka.NotUsed
import akka.actor.typed.{ActorSystem, Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors

object Main {
  def apply(): Behavior[NotUsed] =
    Behaviors.setup { context =>
      val barnRef = context.spawn(Barn(),"barn")
      context.watch(barnRef)
      barnRef ! Barn.StartProduction(dadsNumber = 1030,momsNumber = 1000,booksNumber = 1200,speed = 10000000000f)
      Behaviors.receiveSignal {
        case (_, Terminated(_)) =>
          Behaviors.stopped
      }
    }

  def main(args: Array[String]): Unit = {
    ActorSystem(Main(), "StudentsProduction")
  }

}
