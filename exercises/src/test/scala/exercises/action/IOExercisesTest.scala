package exercises.action

import java.time.Instant

import exercises.action.IOExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class IOExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////
  // 1. Smart constructors
  /////////////////////////

  test("succeed") {
    assert(IO.succeed(4).unsafeRun() == 4)

    forAll { (x: Int) =>
      assert(IO.succeed(x).unsafeRun() == x)
    }
  }

  test("fail") {
    // TODO
  }

  test("effect") {
    forAll { (f: Int => Int, a: Int) =>
      assert(IO.effect(f(a)).unsafeRun() == f(a))
    }
  }

  /////////////////////
  // 2. IO API
  /////////////////////

  test("map") {
    assert(IO.succeed(1).map(_ + 1).unsafeRun() == 2)

    var x = 0
    val bump = IO.effect {
      x += 1; x
    }
    val foo = bump.map(_ * 2)

    assert(x == 0)
    assert(foo.unsafeRun() == 2)
    assert(x == 1)
  }

  test("flatMap") {
    var y = 0
    val fun = (z: Int) => IO.effect {
      y = y + z; y
    }
    val mapped = IO.succeed(2).flatMap(fun)
    assert(y == 0)
    assert(mapped.unsafeRun() == 2)
    assert(y == 2)
  }

  test("attempt") {
    val x = 4
    val err = new Exception("Test")
    assert(IO.succeed(4).attempt.unsafeRun() == Success(x))
    assert(IO(throw err).attempt.unsafeRun() == Failure(err))
  }
  test("handleErrorWith") {
    val x = 4
    val err = new Exception("Test")
    assert(IO.succeed(4).handleErrorWith(_ => IO.pure(5)).unsafeRun() == x)
    assert(IO(throw err).handleErrorWith(_ => IO.pure(5)).unsafeRun() == 5)
  }
  test("retryOnce") {
    assert(IO.succeed(4).retryOnce.unsafeRun() == 4)
    var a = 0
    val throwing = IO.effect {
      if (a == 0) {
        a += 1
        throw new Exception("test")
      }
      else {
        a
      }
    }
    assert(throwing.retryOnce.unsafeRun() == 1)

    val exception = new Exception("TEST")
    val alwaysThrowing = IO.effect { throw exception }
    assert(alwaysThrowing.retryOnce.attempt.unsafeRun() == Failure(exception))
  }
  test("retryUntilSuccess") {
    assert(IO.succeed(4).retryUntilSuccess(10.milliseconds).unsafeRun() == 4)
    var a = 0
    val throwing = IO.effect {
      if (a < 5) {
        a += 1
        throw new Exception("test")
      } else {
        a
      }
    }
    assert(throwing.retryUntilSuccess(1.second).unsafeRun() == 5)
  }
  ////////////////////////
  // 4. Testing
  ////////////////////////

  test("read user from Console") {
    val now = Instant.now()
    val in: ListBuffer[String] = ListBuffer("John", "24")
    val out: ListBuffer[String] = ListBuffer.empty[String]
    val console = testConsole(in, out)
    val clock = testClock(now)

    assert(userConsoleProgram2(console, clock).unsafeRun() == User("John", 24, now))
  }

  ////////////////////////
  // 5. Advanced API
  ////////////////////////

  test("deleteTwoOrders") {}

  test("deleteAllUserOrders") {}

}
