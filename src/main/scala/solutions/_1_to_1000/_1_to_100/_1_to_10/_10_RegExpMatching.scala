package io.github.m4iraki
package solutions._1_to_1000._1_to_100._1_to_10

object _10_RegExpMatching extends Solution[(String, String), Boolean] {

  import scala.annotation.tailrec

  def isMatch(s: String, p: String): Boolean = {
    val sLen = s.length

    def tokenize(pattern: String): Array[String] = {
      val pLen = pattern.length

      @tailrec def loop(idx: Int, acc: List[String]): Array[String] =
        if idx >= pLen then acc.reverse.toArray
        else {
          val isStar = (idx + 1 < pLen) && (pattern(idx + 1) == '*')
          val toIdx = if isStar then idx + 2 else idx + 1
          val token = pattern.substring(idx, toIdx)

          val nextAcc = (acc, token) match {
            case (prev :: tail, t) if prev.length == 2 && t.length == 2 =>
              if (prev == ".*") || (t == ".*") then ".*" :: tail
              else if prev == t then prev :: tail
              else t :: acc
            case (prev :: tail, t) if prev.length == 2 && t.length == 1 =>
              if (prev == ".*") && (t == ".") then ".*" :: "." :: tail
              else if prev(0) == t(0) then prev :: t :: tail
              else t :: acc
            case _ => token :: acc
          }

          loop(toIdx, nextAcc)
        }
      loop(0, Nil)
    }

    val tokens = tokenize(p)
    val tLen = tokens.length

    @tailrec def acceptsEmpty(tIdx: Int): Boolean =
      if tIdx >= tLen then true
      else tokens(tIdx).length == 2 && acceptsEmpty(tIdx + 1)

    @tailrec def loop(
      sIdx: Int,
      tIdx: Int,
      fallbacks: List[(Int, Int)],
    ): Boolean =
      if sIdx >= sLen then {
        if acceptsEmpty(tIdx) then true
        else
          fallbacks match {
            case (fS, fT) :: next => loop(fS, fT, next)
            case Nil              => false
          }
      } else if tIdx >= tLen then {
        fallbacks match {
          case (fS, fT) :: next => loop(fS, fT, next)
          case Nil              => false
        }
      } else {
        val c = s(sIdx)
        val t = tokens(tIdx)

        if t.length == 1 then {
          if (t == ".") || (t(0) == c)
          then loop(sIdx + 1, tIdx + 1, fallbacks)
          else
            fallbacks match {
              case (fS, fT) :: next => loop(fS, fT, next)
              case Nil              => false
            }
        } else if (t(0) == '.') || (t(0) == c)
        then loop(sIdx + 1, tIdx, (sIdx, tIdx + 1) :: fallbacks)
        else loop(sIdx, tIdx + 1, fallbacks)
      }

    loop(0, 0, Nil)
  }

  def naive(s: String, p: String): Boolean = {
    s.matches(p)
  }

  def run: Input => Output = isMatch.tupled

  def samples: Seq[(Input, Output)] = List(
    ("aa", "a") -> false,
    ("aa", "a*") -> true,
    ("ab", ".*") -> true,
  )

}
