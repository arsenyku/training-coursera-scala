package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite
{

  trait TestSets
  {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] =
  {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set")
  {
    new TestSets
    {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5")
  {
    new TestSets
    {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5")
  {
    new TestSets
    {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d")
  {
    new TestSets
    {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)")
  {
    new TestSets
    {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)")
  {
    new TestSets
    {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("most retweets")
  {
    new TestSets
    {
      val t1 = new Tweet("t", "body1", 1)
      val t2 = new Tweet("t", "body2", 2)
      val t3 = new Tweet("t", "body3", 3)
      val t4 = new Tweet("t", "body4", 4)
      val t5 = new Tweet("t", "body5", 5)
      val t6 = new Tweet("t", "body6", 6)
      val t7 = new Tweet("t", "body7", 7)
      val t8 = new Tweet("t", "body8", 8)
      val t9 = new Tweet("t", "body9", 9)
      val low = (new Empty) incl t1 incl t2 incl t3
      val mid = (new Empty) incl t4 incl t5 incl t6
      val high = (new Empty) incl t7 incl t8 incl t9

      assert(low.mostRetweeted.retweets == 3)
      assert(mid.mostRetweeted.retweets == 6)
      assert(high.mostRetweeted.retweets == 9)

    }
  }

  test("more than 50 retweets")
  {
    new TestSets
    {
      val t1 = new Tweet("t", "body1", 10)
      val t2 = new Tweet("t", "body2", 20)
      val t3 = new Tweet("t", "body3", 30)
      val t4 = new Tweet("t", "body4", 40)
      val t5 = new Tweet("t", "body5", 50)
      val t6 = new Tweet("t", "body6", 60)
      val t7 = new Tweet("t", "body7", 70)
      val t8 = new Tweet("t", "body8", 80)
      val t9 = new Tweet("t", "body9", 90)
      val low = (new Empty) incl t1 incl t2 incl t3
      val mid = (new Empty) incl t4 incl t5 incl t6
      val high = (new Empty) incl t7 incl t8 incl t9
      val all = low union mid union high

      assert(size(all.filter(t => t.retweets > 50)) == 4)

    }
  }

  test("descending: set5")
  {
    new TestSets
    {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("union: complexity")
  {
    new TestSets
    {
      val t0 = new Tweet("0", "body0", 0)
      val t1 = new Tweet("1", "body1", 10)
      val t2 = new Tweet("2", "body2", 20)
      val t3 = new Tweet("3", "body3", 30)
      val t4 = new Tweet("4", "body4", 40)
      val t5 = new Tweet("5", "body5", 50)
      val t6 = new Tweet("6", "body6", 60)
      val t7 = new Tweet("7", "body7", 70)
      val t8 = new Tweet("8", "body8", 80)
      val t9 = new Tweet("9", "body9", 90)

//      val low = (new Empty) incl t0 incl t1 incl t2 incl t3 incl t4
//      val high = (new Empty) incl t5 incl t6 incl t7 incl t8 incl t9

      val low = (new Empty) incl t2 incl t4 incl t0 incl t1 incl t3
      val high = (new Empty) incl t7 incl t9 incl t5 incl t6 incl t8

      val all = low union high
    }
  }

}