package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

object _30_SubstringWithConcatenationOfAllWords
  extends Solution[(String, Array[String]), List[Int]] {

  import scala.annotation.tailrec

  object Analyze {
    opaque type AnalysisResult = Int
    val Match: AnalysisResult = 2
    val Possible: AnalysisResult = 1
    val NotMatch: AnalysisResult = 0
    val BadWord: AnalysisResult = -1
    
    inline def apply(
      currentWords: Map[String, Int],
      currentCount: Int,
      update: String,
      targetWords: Map[String, Int],
      targetCount: Int,
    ): AnalysisResult =
      if !targetWords.contains(update) then BadWord
      else {
        val target = targetWords.getOrElse(update, 0)
        val current = currentWords.getOrElse(update, 0) + 1
        if current > target then NotMatch
        else if targetCount == currentCount + 1 then Match
        else Possible
      }

  }

  def include(map: Map[String, Int], value: String): Map[String, Int] =
    map.updatedWith(value) {
      case Some(count) => Some(count + 1)
      case None        => Some(1)
    }

  def remove(map: Map[String, Int], value: String): Map[String, Int] =
    map.updatedWith(value) {
      case Some(count) if count > 1 => Some(count - 1)
      case _                        => None
    }

  def findSubstring(s: String, words: Array[String]): List[Int] = {
    val length = s.length
    val targetCount = words.length
    val wordLength = words.head.length
    val cutoff = length - wordLength * targetCount
    val targetWords =
      words.groupMapReduce(identity)(
        _ => 1,
      )(_ + _)
    
    @tailrec def loop(
      offset: Int,
      wrdCnt: Int,
      map: Map[String, Int],
      acc: List[Int],
    ): List[Int] =
      if offset > cutoff then acc
      else {
        val current = s.substring(
          offset + wrdCnt * wordLength,
          offset + wrdCnt * wordLength + wordLength,
        )
        Analyze(
          currentWords = map,
          currentCount = wrdCnt,
          update = current,
          targetWords = targetWords,
          targetCount = targetCount,
        ) match {
          case Analyze.BadWord =>
            loop(
              offset = offset + wrdCnt * wordLength + wordLength,
              wrdCnt = 0,
              map = Map.empty[String, Int],
              acc = acc,
            )
          case Analyze.Possible =>
            loop(
              offset = offset,
              wrdCnt = wrdCnt + 1,
              map = include(map, current),
              acc = acc,
            )
          case Analyze.Match =>
            loop(
              offset = offset + wordLength,
              wrdCnt = wrdCnt,
              map = remove(
                include(map, current),
                s.substring(offset, offset + wordLength),
              ),
              acc = offset :: acc,
            )
          case Analyze.NotMatch =>
            @tailrec def drop(
              dropped: Int,
              mapAcc: Map[String, Int],
            ): (Int, Map[String, Int]) = {
              val toDrop = s.substring(
                offset + dropped * wordLength,
                offset + dropped * wordLength + wordLength,
              )
              if toDrop == current then (dropped, mapAcc)
              else drop(dropped + 1, remove(mapAcc, toDrop))
            }
            val (dropped, newMap) = drop(0, map)
            loop(
              offset = offset + dropped * wordLength + wordLength,
              wrdCnt = wrdCnt - dropped,
              map = newMap,
              acc = acc,
            )
        }
      }
    (0 until wordLength).toList
      .flatMap {
        loop(_, 0, Map.empty[String, Int], List.empty[Int])
      }
  }

  def naive(s: String, words: Array[String]): List[Int] = {
    val permutations: List[String] =
      words.permutations.map(_.mkString).toList.distinct
    @tailrec
    def allIndexes(from: Int, str: String, acc: List[Int]): List[Int] = {
      val idx = s.indexOf(str, from)
      if idx == -1 then acc
      else allIndexes(idx + 1, str, idx :: acc)
    }
    for {
      permutation <- permutations
      idx <- allIndexes(0, permutation, Nil)
    } yield idx
  }

  def run: ((String, Array[String])) => List[Int] = findSubstring

  def samples: Seq[((String, Array[String]), List[Int])] = List(
    ("barfoothefoobarman", Array("foo", "bar")) -> List(0, 9),
    ("wordgoodgoodgoodbestword", Array("word", "good", "best", "word")) -> Nil,
    ("barfoofoobarthefoobarman", Array("bar", "foo", "the")) -> List(9, 6, 12),
    ("wordgoodgoodgoodbestword", Array("word", "good", "best", "good")) ->
      List(8),
    (
      "pjzkrkevzztxductzzxmxsvwjkxpvukmfjywwetvfnujhweiybwvvsrfequzkhossmootkmyxgjgfordrpapjuunmqnxxdrqrfgkrsjqbszgiqlcfnrpjlcwdrvbumtotzylshdvccdmsqoadfrpsvnwpizlwszrtyclhgilklydbmfhuywotjmktnwrfvizvnmfvvqfiokkdprznnnjycttprkxpuykhmpchiksyucbmtabiqkisgbhxngmhezrrqvayfsxauampdpxtafniiwfvdufhtwajrbkxtjzqjnfocdhekumttuqwovfjrgulhekcpjszyynadxhnttgmnxkduqmmyhzfnjhducesctufqbumxbamalqudeibljgbspeotkgvddcwgxidaiqcvgwykhbysjzlzfbupkqunuqtraxrlptivshhbihtsigtpipguhbhctcvubnhqipncyxfjebdnjyetnlnvmuxhzsdahkrscewabejifmxombiamxvauuitoltyymsarqcuuoezcbqpdaprxmsrickwpgwpsoplhugbikbkotzrtqkscekkgwjycfnvwfgdzogjzjvpcvixnsqsxacfwndzvrwrycwxrcismdhqapoojegggkocyrdtkzmiekhxoppctytvphjynrhtcvxcobxbcjjivtfjiwmduhzjokkbctweqtigwfhzorjlkpuuliaipbtfldinyetoybvugevwvhhhweejogrghllsouipabfafcxnhukcbtmxzshoyyufjhzadhrelweszbfgwpkzlwxkogyogutscvuhcllphshivnoteztpxsaoaacgxyaztuixhunrowzljqfqrahosheukhahhbiaxqzfmmwcjxountkevsvpbzjnilwpoermxrtlfroqoclexxisrdhvfsindffslyekrzwzqkpeocilatftymodgztjgybtyheqgcpwogdcjlnlesefgvimwbxcbzvaibspdjnrpqtyeilkcspknyylbwndvkffmzuriilxagyerjptbgeqgebiaqnvdubrtxibhvakcyotkfonmseszhczapxdlauexehhaireihxsplgdgmxfvaevrbadbwjbdrkfbbjjkgcztkcbwagtcnrtqryuqixtzhaakjlurnumzyovawrcjiwabuwretmdamfkxrgqgcdgbrdbnugzecbgyxxdqmisaqcyjkqrntxqmdrczxbebemcblftxplafnyoxqimkhcykwamvdsxjezkpgdpvopddptdfbprjustquhlazkjfluxrzopqdstulybnqvyknrchbphcarknnhhovweaqawdyxsqsqahkepluypwrzjegqtdoxfgzdkydeoxvrfhxusrujnmjzqrrlxglcmkiykldbiasnhrjbjekystzilrwkzhontwmehrfsrzfaqrbbxncphbzuuxeteshyrveamjsfiaharkcqxefghgceeixkdgkuboupxnwhnfigpkwnqdvzlydpidcljmflbccarbiegsmweklwngvygbqpescpeichmfidgsjmkvkofvkuehsmkkbocgejoiqcnafvuokelwuqsgkyoekaroptuvekfvmtxtqshcwsztkrzwrpabqrrhnlerxjojemcxel",
      Array(
        "dhvf",
        "sind",
        "ffsl",
        "yekr",
        "zwzq",
        "kpeo",
        "cila",
        "tfty",
        "modg",
        "ztjg",
        "ybty",
        "heqg",
        "cpwo",
        "gdcj",
        "lnle",
        "sefg",
        "vimw",
        "bxcb",
      ),
    ) -> List(935),
  )

}
