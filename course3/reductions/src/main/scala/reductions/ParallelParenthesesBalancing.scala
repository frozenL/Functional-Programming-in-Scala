package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def actualBal(chars: Array[Char], cnt: Int): Boolean = chars.isEmpty match {
      case true => cnt == 0
      case false => cnt match {
        case _ if cnt < 0 => false
        case _ => chars.head match {
          case '(' => actualBal(chars.tail, cnt + 1)
          case ')' => actualBal(chars.tail, cnt - 1)
          case _   => actualBal(chars.tail, cnt)
        }
      }
    }
    actualBal(chars, 0)
  }

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until) (arg1, arg2)
      else chars.apply(idx) match {
        case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
        case ')' =>
          if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2)
          else traverse(idx + 1, until, arg1, arg2 + 1)
        case _ => traverse(idx + 1, until, arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val (ret1, ret2) = parallel(reduce(from, mid), reduce(mid, until))
        val minLR = Math.min(ret1._1, ret2._2)
        (ret1._1 - minLR + ret2._1, ret1._2 + ret2._2 - minLR)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }
}
