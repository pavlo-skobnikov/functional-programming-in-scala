package funsets

/** This class is a test suite for the methods in object FunSets.
  *
  * To run this test suite, start "sbt" then run the "test" command.
  */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /** When writing tests, one would often like to re-use certain values for
    * multiple tests. For instance, we would like to create an Int-set and have
    * multiple test about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we
    * can store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes?
    * Then the test methods are not even executed, because creating an instance
    * of the test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    */

  trait TestSets:
    val setOf1 = singletonSet(1)
    val setOf2 = singletonSet(2)
    val setOf3 = singletonSet(3)

    val setOf1And2 = union(setOf1, setOf2)
    val setOf2And3 = union(setOf2, setOf3)

    val setOf1And2And3 = union(union(setOf1, setOf2), setOf3)

    /** This test is currently disabled (by using .ignore) because the method
      * "singletonSet" is not yet implemented and the test would fail.
      *
      * Once you finish your implementation of "singletonSet", remove the
      * .ignore annotation.
      */

    test("singleton set one contains one") {

      /** We create a new instance of the "TestSets" trait, this gives us access
        * to the values "s1" to "s3".
        */
      new TestSets:
        /** The string argument of "assert" is a message that is printed in case
          * the test fails. This helps identifying which assertion failed.
          */
        assert(contains(setOf1, 1), "Singleton")
    }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(setOf1, setOf2)
      assert(contains(s, 1), "Union contains 1")
      assert(contains(s, 2), "Union contains 2")
      assert(!contains(s, 3), "Union doesn't contain 3")
  }

  test("intersect contains only shared elements") {
    new TestSets:
      val intersectedSet = intersect(setOf1And2, setOf2And3)

      assert(!contains(intersectedSet, 1), "Intersect doesn't 1")
      assert(contains(intersectedSet, 2), "Intersect doesn't 2")
      assert(!contains(intersectedSet, 3), "Intersect doesn't 3")
  }

  test("diff contains only elements in first set but not in second") {
    new TestSets:
      val diffedSet = diff(setOf1And2And3, setOf2And3)

      assert(contains(diffedSet, 1), "Diff contains 1")
      assert(!contains(diffedSet, 2), "Diff doesn't contain 2")
      assert(!contains(diffedSet, 3), "Diff doesn't contain 3")
  }

  test("filter contains only elements that satisfy predicate") {
    new TestSets:
      val filteredSet = filter(setOf1And2And3, _ % 2 == 0)

      assert(!contains(filteredSet, 1), "Filter doesn't contain 1")
      assert(contains(filteredSet, 2), "Filter contains 2")
      assert(!contains(filteredSet, 3), "Filter doesn't contain 3")
  }

  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets:
      assert(
        forall(setOf1And2And3, _ < FunSets.bound + 1),
        "Forall 1 is true -> all elements are less than bound"
      )
      assert(
        !forall(setOf1And2And3, _ < 0),
        "Forall 2 is false -> in the given set there are elements that are more than 0"
      )
  }

  test(
    "exists returns whether there exists a bounded integer within `s` that satisfies `p`"
  ) {
    new TestSets:
      assert(
        exists(setOf1And2And3, _ == 2),
        "Exists 1 is true -> 2 is in the given set"
      )
      assert(
        !exists(setOf1And2And3, _ == 4),
        "Exists 2 is false -> 4 is not in the given set"
      )
  }

  test("map returns a set transformed by applying `f` to each element of `s`") {
    new TestSets:
      val mappedSet = map(setOf1And2And3, _ * 2)

      assert(contains(mappedSet, 2), "Map contains 2")
      assert(contains(mappedSet, 4), "Map contains 4")
      assert(contains(mappedSet, 6), "Map contains 6")
      assert(!contains(mappedSet, 1), "Map doesn't contain 1")
      assert(!contains(mappedSet, 3), "Map doesn't contain 3")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
