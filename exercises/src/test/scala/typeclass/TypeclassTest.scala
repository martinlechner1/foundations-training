package typeclass

import exercises.typeclass.{LoggedValue, MyId, Plusable}
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

class TypeclassTest extends FunSuite with Discipline with Matchers {

  test("check Double instance"){
    import exercises.typeclass.TypeclassExercises._
    val p = Plusable[Double]

    p.plus(p.plus(2.0, 3.5), p.zero) shouldEqual 3.5
  }

  test("check Float instance"){
    import exercises.typeclass.TypeclassExercises._
    val p = Plusable[Float]

    p.plus(p.plus(2.0f, 3.5f), p.zero) shouldEqual 3.5f
  }


  test("check MyId instance"){
    import exercises.typeclass.TypeclassExercises._
    val p = Plusable[MyId]

    p.plus(p.plus(MyId("foo"), MyId("bar")), p.zero) shouldEqual MyId("foobar")
  }

  test("check (Int, String) instance"){
    import exercises.typeclass.TypeclassExercises._
    val p = Plusable[(Int, String)]

    p.plus(p.plus((3, "Hello"), (5, "World")), p.zero) shouldEqual ((8, "HelloWorld"))
  }

  test("check List instance"){
    import exercises.typeclass.TypeclassExercises._
    val p = Plusable[List[Int]]

    p.plus(p.plus(List(1,2,3), List(4,5)), p.zero) shouldEqual List(1,2,3,4,5)
  }

  test("check LoggedValue instance"){
    import exercises.typeclass.TypeclassExercises._
    val p = Plusable[LoggedValue[Int]]

    p.plus(p.plus(LoggedValue(1, List("foo")), LoggedValue(5, List("bar"))), p.zero) shouldEqual LoggedValue(6, List("foo", "bar"))
  }

  test("check Option instance"){
    import exercises.typeclass.TypeclassExercises._
    val p = Plusable[Option[Int]]

    p.plus(p.plus(Some(3), Some(4)), p.zero) shouldEqual Some(7)
  }


  test("check instances summonable"){
    "Plusable[Int]" should compile
    "Plusable[Double]" should compile
    "Plusable[Float]" should compile
    "Plusable[MyId]" should compile
    "Plusable[(Int, String)]" should compile
  }

  test("check String instance insert a single space when plus"){
    Plusable[String].plus("Hello", "World") shouldEqual "Hello World"
  }

  checkAll("Int", PlusableLaws[Int])
  checkAll("String", PlusableLaws[String])

  checkAll("Int", PlusableLaws.strong[Int])
}