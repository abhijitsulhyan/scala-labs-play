package org.scalalabs.basic.lab03

import sys._

/**
 * This exercise introduces you to pattern matching in combination with recursion.
 *
 * Recursion is a key concept for the functional style programming.
 * In the exercises below you learn how to apply recursion in combination with Scala's pattern matching facilities.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the corresponding unittest work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching and recursion: http://programming-scala.labs.oreilly.com/ch08.html#Recursion
 */

object RecursionPatternMatchingExercise {

  /**
   * ***********************************************************************
   * Recursive algorithms with pattern matching
   * For expected solution see unittest @RecursionPatternMatchingExerciseTest
   * ***********************************************************************
   */
  /**
   * Create a method that checks that each subsequent value is greater than
   * the previous one.
   * E.g.:
   * checkValuesIncrease(Seq(1,2,3)) == true
   * checkValuesIncrease(Seq(1,2,2)) == false
   */
  def checkValuesIncrease(seq: Seq[Int]): Boolean = {
    def checkValuesIncreaseInner(seq: Seq[Int], increase: Boolean): Boolean = {
      if (increase) {
        seq match {
          case a :: b :: rest => checkValuesIncreaseInner(b :: rest, (a < b))
          case _ => true
        }
      } else {
        true
      }
    }

    checkValuesIncreaseInner(seq, true)
  }

  /**
   * Group Consecutive values
   * List(1,1,2,3,1,1) -> List(1,1), List(2), List(3), List(1,1)
   */
  def groupConsecutive[T](in: List[T]): List[List[T]] = in match {
    case Nil => Nil
    case head :: tail => {
      // consecutive 'use' span to check each condition till it breaks // takeWhile // dropWhile
      val (iHead, iTail) = in.span(_ == head)
      iHead :: groupConsecutive(iTail)
    }
  }

  /**
   * Group Equal values
   * List(1,1,2,3,1,1) -> List(1,1,1,1), List(2), List(3)
   */
  def groupEquals[T](in: List[T]): List[List[T]] = in match {
    case Nil => Nil
    case head :: tail => {
      // 'partition' groups elements as combination of filter / filterNot
      val (iHead, iTail) = in.partition(_ == head)
      iHead :: groupEquals(iTail)
    }
  }

  /**
   * Compress values
   * List(1,1,2,3,1,1) -> List(1,2,3)
   */
  def compress[T](in: List[T]): List[T] = in match {
    case Nil => Nil
    case a :: b :: rest if (a == b) => a :: compress(rest)
    case a :: rest => a :: compress(rest)
  }

  /**
   * Define the amount of all equal members
   * List(1,1,2,3,1,1) -> List((4,1),(1,2),(1,3))
   */
  def amountEqualMembers[T](in: List[T]): List[(Int, T)] = {
    groupEquals(in).map(l => (l.size, l.head))
  }

  /**
   * Zip multiple lists
   * List(List(1,2,3), List('A, 'B, 'C), List('a, 'b, 'c)) -> List(List(1, 'A, 'a), List(2, 'B, 'b), List(3, 'C, 'c))
   */
  def zipMultiple(in: List[List[_]]): List[List[_]] = {

    def zipAll(in: List[List[_]]): List[List[_]] = in match {
      case Nil => Nil
      case list => add(list) :: zipAll(remove(list))
    }

    def add(in: List[List[_]]): List[_] = in match {
      case Nil => Nil
      case head :: tail => head.head :: add(tail)
    }

    def remove(in: List[List[_]]): List[List[_]] = in match {
      case Nil => Nil
      case head :: tail => head.tail :: remove(tail)
    }

    zipAll(in)
  }

  /**
   * Zip multiple lists with different sizes
   * List(List(1), List('A, 'B, 'C), List('a, 'b)) -> List(List(1, 'A, 'a))
   */
  def zipMultipleWithDifferentSize(in: List[List[_]]): List[List[_]] = {
    val minLen = in.sortBy(_.size).head.size
    def dropExtraSizeList(in: List[List[_]], maxLen: Int): List[List[_]] = {
      in.map(innerList => innerList.take(maxLen))
    }
    zipMultiple(dropExtraSizeList(in, minLen))
  }

}
