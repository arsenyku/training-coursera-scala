package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val s8 = singletonSet(8)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("forall returns true if all elements match the predicate") {
    new TestSets {
      val t = union(s1,union(s2,union(s3,s4)))

      val allUnder5 = forall(t, x => x < 5)
      assert (allUnder5, "All under 5 should be true")
    }
  }

  test("forall returns false if all elements do not match the predicate") {
    new TestSets {
      val t = union(s1,union(s2,union(s3,s4)))

      val allOver5 = forall(t, x => x > 5)
      assert (!allOver5, "All over 5 should be false")
    }
  }

  test("forall returns false if some elements do not match the predicate") {
    new TestSets {
      val t = union(s1,union(s2,union(s3,s4)))

      val allEven = forall(t, x => x % 2 == 0)
      assert (!allEven, "All even should be false")
    }
  }

  test("add 1 to all returns the correct set") {
    new TestSets {
      val t = union(s1,union(s2,union(s3,s4)))
      val expected = union(s2,union(s3,union(s4,s5)))

      val result = map(t, x => x + 1)
      val allOver1 = forall(result, x => x > 1)
      val allUnder6 = forall(result, x => x <6)
      assert (allOver1 && allUnder6, "Elements in resulting set should be 2 to 5")
      assert (forall(result, x => contains(expected,x)), "Elements in resulting set should be in expected set")
      assert (forall(expected, x => contains(result,x)), "Elements in expected set should be in result set")
    }
  }

  test("all times 2 returns the correct set") {
    new TestSets {
      val t = union(s1,union(s2,union(s3,s4)))
      val expected = union(s2,union(s4,union(s6,s8)))

      val result = map(t, x => x * 2)
      val allOver1 = forall(result, x => x > 1)
      val allUnder9 = forall(result, x => x < 9)
      assert (allOver1 && allUnder9, "Elements in resulting set should be 2 to 8")
      assert (forall(result, x => contains(expected,x)), "Elements in resulting set should be in expected set")
      assert (forall(expected, x => contains(result,x)), "Elements in expected set should be in result set")
    }
  }
}
