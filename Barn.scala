package StudentsProduction

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object Barn {
  private var swags = 0
  private var dads = 0
  private var moms = 0
  private var books = 0
  private var babies = 0
  private var graduates = 0
  private var students = 0
  private var speed = 1f

  private var finishedStork = false;
  private var finishedHospital = false;
  private var finishedSchool = false;
  private var finishedParadigms = false;

  sealed trait Control
  final case class StartProduction(swagsNumber: Int, dadsNumber: Int, momsNumber : Int, booksNumber : Int, speed : Float) extends Control
  final case class AskForPreBabyResources(hospital: ActorRef[Produce]) extends Control
  final case class AskForBabyResources(hospital: ActorRef[Produce]) extends Control
  final case class AskForGraduateResources(school: ActorRef[Produce]) extends Control
  final case class AskForStudentResources(pp: ActorRef[Produce]) extends Control
  final case class AddPreBaby(school : ActorRef[Produce]) extends Control
  final case class AddBaby(school : ActorRef[Produce]) extends Control
  final case class AddGraduate(programmingParadigms : ActorRef[Produce]) extends Control
  final case class AddStudent() extends Control

  sealed trait Produce
  final case class ProduceOneInstance(barn: ActorRef[Control]) extends Produce
  final case class AskForAvailableSlot(barn: ActorRef[Control]) extends Produce


  def apply(): Behavior[Control] =
    {
      Behaviors.receive{ (context,message) =>
        message match {
          case StartProduction(newSwagsNumber, newDadsNumber,newMomsNumber,newBooksNumber,newSpeed) =>
            swags = newSwagsNumber
            dads = newDadsNumber
            moms = newMomsNumber
            books = newBooksNumber
            speed = newSpeed
            val programmingParadigms = context.spawn(ProgrammingParadigms(), "pp")
            val school = context.spawn(School(programmingParadigms), "school")
            val hospital = context.spawn(Hospital(school),"hospital")
            val stork = context.spawn(Stork(hospital),"stork")
            stork ! AskForAvailableSlot(context.self)
            Behaviors.same
          case AskForPreBabyResources(stork) =>
            if (hasPreBabyResources)
              {
                finishedStork = false;
                takeSwag
                context.log.info("barn sends resources to stork")
                stork ! ProduceOneInstance(context.self)
              }
            else finishedStork = true;
              Behaviors.same
          case AskForBabyResources(hospital) =>
            if (hasBabyResources)
              {
                finishedHospital = false;
                takeDad
                takeMom
                context.log.info("barn sends resources to hospital")
                hospital ! ProduceOneInstance(context.self)
              }
            else finishedHospital = true;
            Behaviors.same
          case AskForGraduateResources(school) =>
            if (hasGraduateResources) {
              finishedSchool = false;
              takeBaby
              takeBook
              context.log.info("barn sends resources to school")
              school ! ProduceOneInstance(context.self)
            }
            else finishedSchool = true;
            Behaviors.same
          case AskForStudentResources(pp) =>
            if (hasStudentResources)
              {
                finishedParadigms = true;
                takeGraduate
                context.log.info("barn sends resources to pp")
                pp ! ProduceOneInstance(context.self)
                Behaviors.same
              }
            else if (isFinished) {
              context.log.info("FINISHED! CREATED: {} students\n", students)
              Behaviors.stopped
            }
            else Behaviors.same
          case AddPreBaby(hospital) =>
            context.log.info("baby started")
            hospital ! AskForAvailableSlot(context.self)
            Behaviors.same
          case AddBaby(school) =>
            context.log.info("baby added")
            babies = babies + 1
            school ! AskForAvailableSlot(context.self)
            Behaviors.same
          case AddGraduate(programmingParadigms) =>
            context.log.info("graduate added")
            graduates = graduates + 1
            programmingParadigms ! AskForAvailableSlot(context.self)
            Behaviors.same
          case AddStudent() =>
            context.log.info("student added")
            students = students + 1
            Behaviors.same
        }
      }
    }

  private def isFinished : Boolean = finishedHospital && finishedSchool

  private def hasPreBabyResources: Boolean = swags > 0
  private def hasBabyResources: Boolean = dads > 0 && moms > 0
  private def hasGraduateResources: Boolean = babies > 0 && books > 0
  private def hasStudentResources: Boolean = graduates > 0

  private def takeSwag = swags = swags - 1
  private def takeDad = dads = dads - 1
  private def takeMom = moms = moms - 1
  private def takeBaby = babies = babies - 1
  private def takeBook = books = books - 1
  private def takeGraduate = graduates = graduates - 1

  def getSpeed: Float = speed
}
