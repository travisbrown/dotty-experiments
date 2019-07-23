package io.circe.cats.laws

import io.circe.cats.arrow.Strong
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.profunctor._
import given io.circe.cats.syntax.strong._

/**
 * Laws that must be obeyed by any `cats.functor.Strong`.
 *
 * See: [[https://arxiv.org/abs/1406.4823 E. Rivas, M. Jaskelioff Notions of Computation as Monoids, Chapter 7]]
 * See: [[http://hackage.haskell.org/package/profunctors/docs/Data-Profunctor-Strong.html Haskell Data.Profunctor.Strong]]
 */
trait StrongLaws[F[_, _]] given (F: Strong[F]) extends ProfunctorLaws[F] {
  private def swapTuple[X, Y]: Tuple2[X, Y] => Tuple2[Y, X] = _.swap

  /** first' == dimap swap swap . second' */
  def firstIsSwappedSecond[A, B, C](fab: F[A, B]): IsEq[F[(A, C), (B, C)]] =
    F.first[A, B, C](fab) <-> F.second[A, B, C](fab).dimap(swapTuple[A, C])(swapTuple[C, B])

  /** second' == dimap swap swap . first' */
  def secondIsSwappedFirst[A, B, C](fab: F[A, B]): IsEq[F[(C, A), (C, B)]] =
    F.second[A, B, C](fab) <-> F.first[A, B, C](fab).dimap(swapTuple[C, A])(swapTuple[B, C])

  /** lmap fst == rmap fst . first' */
  def lmapEqualsFirstAndThenRmap[A, B, C](fab: F[A, B]): IsEq[F[(A, C), B]] =
    fab.lmap[(A, C)]({ case (a, _) => a }) <-> F.first[A, B, C](fab).rmap[B](_._1)

  /** lmap snd == rmap snd . second' */
  def lmapEqualsSecondAndThenRmap[A, B, C](fab: F[A, B]): IsEq[F[(C, A), B]] =
    fab.lmap[(C, A)]({ case (_, b) => b }) <-> F.second[A, B, C](fab).rmap[B](_._2)

  private def mapFirst[X, Y, Z](f: X => Z)(cb: (X, Y)): (Z, Y) = (f(cb._1), cb._2)
  private def mapSecond[X, Y, Z](f: Y => Z)(cb: (X, Y)): (X, Z) = (cb._1, f(cb._2))

  /** lmap (second f) . first == rmap (second f) . first */
  def dinaturalityFirst[A, B, C, D](fab: F[A, B], f: C => D): IsEq[F[(A, C), (B, D)]] =
    F.first[A, B, C](fab).rmap(mapSecond(f)) <-> F.first[A, B, D](fab).lmap(mapSecond(f))

  /** lmap (first f) . second == rmap (first f) . second */
  def dinaturalitySecond[A, B, C, D](fab: F[A, B], f: C => D): IsEq[F[(C, A), (D, B)]] =
    F.second[A, B, C](fab).rmap(mapFirst(f)) <-> F.second[A, B, D](fab).lmap(mapFirst(f))

  private def assoc[A, B, C]: (((A, B), C)) => (A, (B, C)) = { case ((a, c), d)   => (a, (c, d)) }
  private def unassoc[A, B, C]: ((A, (B, C))) => ((A, B), C) = { case (a, (c, d)) => ((a, c), d) }

  /** first' . first' == dimap assoc unassoc . first' where
   *   assoc ((a,b),c) = (a,(b,c))
   *   unassoc (a,(b,c)) = ((a,b),c)
   */
  def firstFirstIsDimap[A, B, C, D](fab: F[A, B]): IsEq[F[((A, C), D), ((B, C), D)]] =
    F.first[(A, C), (B, C), D](F.first[A, B, C](fab)) <-> F.first[A, B, (C, D)](fab).dimap[((A, C), D), ((B, C), D)](assoc)(unassoc)

  /** second' . second' == dimap unassoc assoc . second' where
   *   assoc ((a,b),c) = (a,(b,c))
   *   unassoc (a,(b,c)) = ((a,b),c)
   */
  def secondSecondIsDimap[A, B, C, D](fab: F[A, B]): IsEq[F[(D, (C, A)), (D, (C, B))]] =
    F.second[(C, A), (C, B), D](F.second[A, B, C](fab)) <-> F.second[A, B, (D, C)](fab).dimap[(D, (C, A)), (D, (C, B))](unassoc)(assoc)
}

object StrongLaws {
  def apply[F[_, _]] given Strong[F]: StrongLaws[F] =
    new StrongLaws[F] with ProfunctorLaws[F]
}
