package scala.slick.additions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import States._
import scala.slick.lifted.BaseTypeMapper

class KeyedTableTests extends FunSuite with ShouldMatchers {
  object driver extends scala.slick.driver.H2Driver with KeyedTableComponent
  import driver.simple._

  case class DbIO[A](apply: Session => A)
  class DbMod[A](f: A => (A, DbIO[Unit])) {
    def apply(s: A): (A, DbIO[Unit]) = f(s)

    def andThen(that: DbMod[A]) = new DbMod({ a: A =>
      val (thisA, thisD) = this.apply(a)
      val (thatA, thatD) = that.apply(thisA)
      (thatA, DbIO(s => { println("Here"); thisD.apply(s); thatD.apply(s) }))
    })
  }

  trait Lenses[K, A] extends EntityTable[K, A] {
    class ColLens[C](val col: Lenses.this.type => Column[C])(lens: Lens[A, C]) {
      def get = lens.get
      def set: (A, C) => A = lens.set

      def update(f: C => C)(implicit btm: BaseTypeMapper[K]) = UpdateCol(this)(f)
    }

    case class UpdateCol[C](tlens: ColLens[C])(f: C => C)(implicit btm: BaseTypeMapper[K]) extends DbMod[KEnt]({ e: KEnt =>
      val newValue = f(tlens.get(e.value))
      println(s"Computing State for $e")
      (
        e.map(tlens.set(_, newValue)),
        DbIO{ implicit session =>
          println("Setting value to " + newValue)
          Query(this).where(_.key is e.key) map (t => tlens.col(t.asInstanceOf[Lenses.this.type])) update newValue
        }
      )
    })
  }

  case class Phone(kind: String, number: String)
  object Phones extends Table[(People.Lookup, String, String)]("phones") {
    def person = column[People.Lookup]("personid")
    def kind = column[String]("kind")
    def number = column[String]("number")
    def mapping = ??? //person ~ kind ~ number <-> (Phone.apply _, Phone.unapply _)
    override def * = person ~ kind ~ number
  }

  case class Person(first: String, last: String)
  object People extends EntityTable[Long, Person]("people") with Lenses[Long, Person] {
    def first = column[String]("first")
    def last = column[String]("last")

    val lastC = new ColLens(_.last)(Lens(_.last, (p, x) => p.copy(last = x)))

    def mapping = first ~ last <-> (Person.apply _, Person.unapply _)

    /*def find(k: Long)(implicit session: Session) = (for {
      (people, phones) <- People leftJoin Phones on (_.lookup is _.person)
      if people.key is k
    } yield ((people.first, people.last), phones))
      .list
      .groupBy{ t: ((String, String), Phones.KEnt) => t._1 }
      .toSeq
      .headOption
      .map { case ((f, l), _) => Person(f, l) }*/
  }

  val db = Database.forURL("jdbc:h2:test", driver = "org.h2.Driver")

  db.withSession { implicit session: Session =>
    println(scala.util.Try((Phones.ddl ++ People.ddl).create))
    //  (Phones.ddl ++ People.ddl).createStatements foreach println
  }

  test("Test one") {

    val person = Person("First", "Last")
    case class DiffSeq[A](initial: Seq[A], removed: Seq[A], added: Seq[A], changed: Seq[A => A])

    //    case class PhonesState(person: People.Ent, phones: Seq[Phones.Ent], apply: Session => Unit)
    db.withSession { implicit session: Session =>
      /*lazy val lens1: Lens[People.Ent, Seq[Phones.Ent]] = Lens(
        {
          case e: KeylessEntity[_, _] => Nil
          case ke: KeyedEntity[_, _]  => Query(Phones).where(_.key is ke.key).list
        },
        (person, phones) => {
          val old = lens1.get(person)
          def newItems = phones filterNot old.contains
          def removedItems = old filterNot phones.contains
          def replacedItems = phones flatMap {
            case c: KeyedEntity[_, _] =>
              old.find{
                case i: KeyedEntity[_, _] => i.key == c.key && (i.value != c.value)
              }.map(i => (i, c))
            case _ =>
              Nil
          }

          ???
        }
      )*/
      val test1 = People.lastC.update(_ + ", updated once") andThen People.lastC.update(_ + ", updated again")
      val result = test1 apply SavedEntity(10L, Person("First", "Last1"))
      println("Result: " + result)
      db withSession result._2.apply
    }
  }
}
