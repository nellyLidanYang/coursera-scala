package forcomp


object Anagrams {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = (w.toLowerCase groupBy(_.toChar)).toList.sorted map ({case (x, y) => (x, y.length)})

  def addOccurrence(occurrences: Anagrams.Occurrences, occurrences1: Anagrams.Occurrences): Occurrences = {
    val occurmap = occurrences.toMap withDefaultValue 0
    val occurmap1 = occurrences1.toMap
    def adjust(term: (Char, Int)): (Char, Int) = {
      val (exp, freq) = term
      exp -> (freq + occurmap(exp))
    }
    (occurmap ++ (occurmap1 map adjust)).toList.sorted
  }

  def sentenceOccurrences(s: Sentence): Occurrences = s match {
    case List() => List()
    case w :: ws => addOccurrence(wordOccurrences(w), sentenceOccurrences(ws))
  }

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = (dictionary groupBy (wordOccurrences(_))) withDefaultValue List[Word]()

  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def gen(c: Char, i: Int) = if (i == 0) List() else List((c, i))
    occurrences match {
    case List() => List(occurrences)
    case (k, v) :: occ =>
      (for {
        iterv <- 0 to v
        oc <- combinations(occ)
      } yield gen(k, iterv) ::: oc).toList
  }}

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def sub(tt: Map[Char, Int], t: (Char, Int)): Map[Char, Int] = {
      val (k, v) = t
      tt + (k -> (tt(k) - v))
    }
    ((y.toMap foldLeft x.toMap)(sub)).toList.sorted filter {case (_, v) => v > 0}
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val occur = sentenceOccurrences(sentence)
    def helper(ocr: Occurrences): List[Sentence] = {
      ocr match {
        case List() => List(Nil)
        case _ =>
          for {
            cur <- combinations(ocr)
            w <- dictionaryByOccurrences(cur)
            remain <- helper(subtract(ocr, cur))
            if (cur != List(Nil) && cur != occur && dictionaryByOccurrences(cur).length != 0)
          } yield w :: remain
      }
    }
    helper(occur)
  }
}
