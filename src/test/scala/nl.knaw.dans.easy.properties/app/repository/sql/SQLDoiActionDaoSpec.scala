package nl.knaw.dans.easy.properties.app.repository.sql

import java.util.UUID

import cats.scalatest.EitherValues
import nl.knaw.dans.easy.properties.app.model.{ DoiAction, DoiActionEvent }
import nl.knaw.dans.easy.properties.app.repository.NoSuchDepositError
import nl.knaw.dans.easy.properties.fixture.{ DatabaseDataFixture, DatabaseFixture, FileSystemSupport, TestSupportFixture }
import org.joda.time.DateTime

class SQLDoiActionDaoSpec extends TestSupportFixture
  with FileSystemSupport
  with DatabaseFixture
  with DatabaseDataFixture
  with EitherValues {

  "getCurrent" should "return the current doi action event of the given deposits" in {
    val doiActions = new SQLDoiActionDao

    doiActions.getCurrent(Seq(depositId1, depositId5)).value should contain only(
      depositId1 -> Some(doiAction1),
      depositId5 -> Some(doiAction5),
    )
  }

  it should "return a None if the depositId is unknown" in {
    val doiActions = new SQLDoiActionDao
    val depositId6 = UUID.fromString("00000000-0000-0000-0000-000000000006")

    doiActions.getCurrent(Seq(depositId6)).value should contain only (depositId6 -> Option.empty)
  }

  it should "return an empty collection when the input collection is empty" in {
    val doiActions = new SQLDoiActionDao

    doiActions.getCurrent(Seq.empty).value shouldBe empty
  }

  "getAll" should "return all doi action events associated with the given deposits" in {
    val doiActions = new SQLDoiActionDao

    doiActions.getAll(Seq(depositId1, depositId4)).value should contain only(
      depositId1 -> Seq(doiAction0, doiAction1),
      depositId4 -> Seq(doiAction4),
    )
  }

  it should "return a None if the depositId is unknown" in {
    val doiActions = new SQLDoiActionDao
    val depositId6 = UUID.fromString("00000000-0000-0000-0000-000000000006")

    doiActions.getAll(Seq(depositId6)).value should contain only (depositId6 -> Seq.empty)
  }

  it should "return an empty collection when the input collection is empty" in {
    val doiActions = new SQLDoiActionDao

    doiActions.getAll(Seq.empty).value shouldBe empty
  }

  "store" should "insert a new doi action event into the database" in {
    val doiActions = new SQLDoiActionDao
    val timestamp = new DateTime(2019, 7, 19, 22, 45, timeZone)
    val doiActionEvent = DoiActionEvent(DoiAction.UPDATE, timestamp)

    doiActions.store(depositId5, doiActionEvent).value shouldBe doiActionEvent
    doiActions.getCurrent(Seq(depositId5)).value should contain only (depositId5 -> Some(doiActionEvent))
    doiActions.getAll(Seq(depositId5)).value.toMap.apply(depositId5) should contain only(doiAction5, doiActionEvent)
  }

  it should "fail when the given depositId does not exist" in {
    val doiActions = new SQLDoiActionDao
    val depositId6 = UUID.fromString("00000000-0000-0000-0000-000000000006")
    val timestamp = new DateTime(2019, 7, 18, 22, 38, timeZone)
    val doiActionEvent = DoiActionEvent(DoiAction.CREATE, timestamp)

    doiActions.store(depositId6, doiActionEvent).leftValue shouldBe NoSuchDepositError(depositId6)
  }
}