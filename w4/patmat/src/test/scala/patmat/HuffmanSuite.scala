package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    val timesResult0 = times(List('a', 'b', 'a'))
    assert(timesResult0.length == 2)

    val timesResult1 = times(List('h', 'e', 'l', 'l', 'o', '-', '+', 'w', 'o', 'r', 'l', 'd'))
    //println(timesResult1)
    assert(timesResult1.length == 9)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("tree from char list") {
    new TestTrees {
      val tree = createCodeTree(List('a','b','b','c','c','c','c'))
      assert(tree === Fork(Fork(Leaf('a',1),Leaf('b',2),List('a', 'b'),3),Leaf('c',4),List('a', 'b', 'c'),7))
    }
  }

  test("decode") {
    new TestTrees {
      val tree = createCodeTree(List('A','B','B','C','C','C','C'))
      val decoded = decode(tree, List(1,0,0,0,1))
      println(decoded)
      assert(decoded === "CAB".toList)
    }
  }

  test("decoded french secret") {
    new TestTrees {
      assert(decodedSecret.mkString === "huffmanestcool")
    }
  }

  test("build code table") {
    val tree = createCodeTree(List('A','B','B','C','C','C','C'))
    val codeTable = convert(tree)
    assert(codeTable === List(('A',List(0, 0)), ('B',List(0, 1)), ('C',List(1))))
  }

  test("encode") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
