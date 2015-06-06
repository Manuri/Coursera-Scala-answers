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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
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
    val s4 = Set(1,2,3)
    val s5 = Set(1,3,5)
    val s6 = Set(0,1,2,3,4,5,6)
    val s7 = Set(-4,0,2,4,6,1005,-2000)
    val s8 = Set(-4,0,2,4,6,1005,-2000,3)
    val s9 = Set(-3,5,7,-2000,1002,17)
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

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only intersection of elements"){
    new TestSets {
      val s= intersect(s4,s5)

      assert(contains(s,1),"intersection 1")
      assert(!contains(s,2),"intersection 2")
      assert(contains(s,3),"intersection 3")
      assert(!contains(s,5),"intersection 5")
    }
  }

  test("difference contains only the difference of the two given sets"){
    new TestSets {
      val s= diff(s4,s5)

      assert(!contains(s,1),"difference 1")
      assert(contains(s,2),"difference 2")
      assert(!contains(s,3),"difference 3")
      assert(!contains(s,5),"difference 5")
    }
  }

  test("should return only the subset for which p holds"){
    new TestSets {
      val s=filter(s6,x=>x%2==0)

      assert(contains(s,0),"filer 0")
      assert(!contains(s,1),"filer 1")
      assert(contains(s,2),"filer 2")
      assert(!contains(s,3),"filer 3")
      assert(contains(s,4),"filer 4")
      assert(!contains(s,5),"filer 5")
    }
  }

  test("all bounded integers within `s` should satisfy `p`"){
    new TestSets {
      assert(forall(s7,x=>(x%2==0))==true ,"for all s7")
      assert(forall(s8,x=>(x%2==0))==false, "for all s8")
    }
  }

  test("check whether it exists"){
    new TestSets {
      assert(exists(s8,x=>(x%2==0))==true,"exists s8")
      assert(exists(s9,x=>(x%2==0))==false,"exists s9")
    }
  }

  test("check if belongs to the mapped set"){
    new TestSets {
      val s=map(s6,x=>4*x)

      assert(contains(s,4),"map 4")
      assert(!contains(s,5),"map 5")
    }
  }


}
