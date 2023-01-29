package StudentsProduction

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object Barn {
  private var dadsNumber = 0
  private var momsNumber = 0
  private var booksNumber = 0
  private var babiesNumber = 0
  private var graduatesNumber = 0
  private var studentsNumber = 0
  private var speed = 1f
  private var finishedHospital = false
  private var finishedSchool = false
  private var finishedParadims = false
  sealed trait Control
  final case class StartProduction(dadsNumber: Int, momsNumber : Int, booksNumber : Int, speed : Float) extends Control
  final case class AskForBabyResources(hospital: ActorRef[Produce]) extends Control
  final case class AskForGraduateResources(school: ActorRef[Produce]) extends Control
  final case class AskForStudentResources(pp: ActorRef[Produce]) extends Control
  final case class AddBaby(school : ActorRef[Produce]) extends Control
  final case class AddGraduate(programmingParadigms : ActorRef[Produce]) extends Control
  final case class AddStudent() extends Control
  final case class FinishedHospital() extends Control
  final case class ActiveHospital() extends Control
  final case class FinishedSchool() extends Control
  final case class ActiveSchool() extends Control
  final case class FinishedParadims() extends Control
  final case class ActiveParadims() extends Control

  sealed trait Produce
  final case class ProduceOneInstance(barn: ActorRef[Control]) extends Produce
  final case class AskForAvailableSlot(barn: ActorRef[Control]) extends Produce
  final case class CheckIfFinished(barn: ActorRef[Control]) extends Produce


  def apply(): Behavior[Control] =
    {
      Behaviors.receive{ (context,message) =>
        message match {
          case StartProduction(newDadsNumber,newMomsNumber,newBooksNumber,newSpeed) =>
            dadsNumber = newDadsNumber
            momsNumber = newMomsNumber
            booksNumber = newBooksNumber
            speed = newSpeed
            val programmingParadigms = context.spawn(ProgrammingParadigms(), "pp")
            val school = context.spawn(School(programmingParadigms), "school")
            val hospital = context.spawn(Hospital(school),"hospital")
            hospital ! AskForAvailableSlot(context.self)
            Behaviors.same
          case AskForBabyResources(hospital) =>
            if (hasBabyResources)
              {
                finishedHospital = false
                getDad
                getMom
                context.log.info("barn sends resources to hospital")
                hospital ! ProduceOneInstance(context.self)
                Behaviors.same
              }
            else if (isFinished)
            {
              context.log.info("HOSPITALthat's it, created: {} students", studentsNumber)
              Behaviors.stopped
            }
            else {
              Behaviors.same
            }
          case AskForGraduateResources(school) =>
            if (hasGraduateResources) {
              finishedSchool = false
              getBaby
              getBook
              context.log.info("barn sends resources to school")
              school ! ProduceOneInstance(context.self)
              Behaviors.same
            }
            else if (isFinished)
            {
              context.log.info("SCHOOLthat's it, created: {} students", studentsNumber)
              Behaviors.stopped
            }
            else
              {
                Behaviors.same
              }
          case AskForStudentResources(pp) =>
            if (hasStudentResources)
              {
                finishedParadims = false
                getGraduate
                context.log.info("barn sends resources to pp")
                pp ! ProduceOneInstance(context.self)
                Behaviors.same
              }
            else if (isFinished)
            {
              context.log.info("PPthat's it, created: {} students", studentsNumber)
              Behaviors.stopped
            }
            else Behaviors.same
          case AddBaby(school) =>
            context.log.info("baby added")
            babiesNumber = babiesNumber + 1
            school ! AskForAvailableSlot(context.self)
            Behaviors.same
          case AddGraduate(programmingParadigms) =>
            context.log.info("graduate added")
            graduatesNumber = graduatesNumber + 1
            programmingParadigms ! AskForAvailableSlot(context.self)
            Behaviors.same
          case AddStudent() =>
            context.log.info("student added")
            studentsNumber = studentsNumber + 1
            context.log.info("CREATED: {} students\n", studentsNumber)
            Behaviors.same
          case FinishedHospital() => finishedHospital = true;Behaviors.same
          case FinishedSchool() => finishedSchool = true;Behaviors.same
          case FinishedParadims() => finishedParadims = true;Behaviors.same
          case ActiveHospital() => finishedHospital = false;Behaviors.same
          case ActiveSchool() => finishedSchool = false;Behaviors.same
          case ActiveParadims() => finishedParadims = false;Behaviors.same
        }
      }
    }

  private def isFinished: Boolean = {
    false
    //finishedHospital && finishedSchool && finishedParadims && !hasBabyResources && !hasGraduateResources && !hasStudentResources
  }

  private def hasBabyResources: Boolean = dadsNumber > 0 && momsNumber > 0
  private def hasGraduateResources: Boolean = babiesNumber > 0 && booksNumber > 0
  private def hasStudentResources: Boolean = graduatesNumber > 0

  private def getDad = dadsNumber = dadsNumber - 1
  private def getMom = momsNumber = momsNumber - 1
  private def getBaby = babiesNumber = babiesNumber - 1
  private def getBook = booksNumber = booksNumber - 1
  private def getGraduate = graduatesNumber = graduatesNumber - 1

  def getSpeed: Float = speed
}
