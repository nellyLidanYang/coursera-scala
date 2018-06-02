package patmat

import common._

object Huffman {
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

    def weight(tree: CodeTree): Int = tree match{
      case Leaf(_, w) => w
      case Fork(_, _, _, w) => w
    }
  
    def chars(tree: CodeTree): List[Char] = tree match {
      case Leaf(c, _) => List[Char](c)
      case Fork(l, r, _, _) => chars(l) ::: chars(r)
    }
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList
// infinite loop
    def times(chars: List[Char]): List[(Char, Int)] = {
//      def helper(cx: List[Char], acc: Map[Char, Int]): Map[Char, Int] = cx match {
//        case Nil => acc
//        case x :: lx =>
//          val t = acc getOrElse(x, 0)
//          acc + (x -> (t + 1))
//          helper(lx, acc)
//      }
//      val res = helper(chars, Map[Char, Int]())
//      res.map { case (k,v) => (k, v) }(collection.breakOut): List[(Char, Int)]
      chars.groupBy(identity).mapValues(_.size).toList
    }

    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      freqs.map(item => Leaf(item._1, item._2)).sortWith((one, two) => weight(one) < weight(two))
    }
//      freqs match {
//      case Nil => List()
//      case f :: fs => (new Leaf(f._1, f._2) :: makeOrderedLeafList(fs)).sortWith((one, two) => weight(one) < weight(two))
//    }

    def singleton(trees: List[CodeTree]): Boolean = trees match {
      case _ :: Nil => true
      case _ => false
    }

    def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
      case x :: y :: cs => (makeCodeTree(x, y) :: cs).sortWith((one, two) => weight(one) < weight(two))
      case _ => trees
    }

    def until(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
      if (singleton(trees)) trees
      else until(singleton, combine)(combine(trees))
    }

    def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  

  // Part 3: Decoding

  type Bit = Int

    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
      def helper(ts: CodeTree, bs: List[Bit]): List[Char] = ts match {
        case Leaf(c, _) => if (bs.isEmpty) List(c) else c :: helper(tree, bs)
        case Fork(l ,r, _, _) => if (bs.head == 0) helper(l, bs.tail) else helper(r, bs.tail)
      }
      helper(tree, bits)
    }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
      def helper(ts: CodeTree)(c: Char): List[Bit] = ts match {
        case Leaf(c, _) => List[Bit]()
        case Fork(l, r, _, _) => if (chars(l).contains(c)) 0 :: helper(l)(c) else 1 :: helper(r)(c)
      }
      text.flatMap(helper(tree))
    }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]
// maybe infinite loop
    def codeBits(table: CodeTable)(char: Char): List[Bit] = {
      table.find(item => item._1 == char).get._2
    }
//      table match {
//      case Nil => throw new NoSuchElementException("Empty CodeTable")
//      case x :: xs => if (x._1 == char) x._2 else codeBits(xs)(char)
//    }
//

    def convert(tree: CodeTree): CodeTable = tree match {
      case Leaf(c, _) => List((c, List()))
      case Fork(l, r, c, _) => mergeCodeTables(convert(l), convert(r))
    }

    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
      def helper(bi: Bit, ls: CodeTable): CodeTable = ls.map(item => (item._1, bi :: item._2))
      helper(0, a) ::: helper(1, b)
    }

    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = text.flatMap(codeBits(convert(tree)))
  }
