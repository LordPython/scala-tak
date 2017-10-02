package org.josephmartin.libtak.util

import monocle.{PLens, Traversal}

import scalaz.State

object ModifyWithState {
  implicit final class PLensModifyWithState[S, T, A, B](val traversal: PLens[S, T, A, B]) extends AnyVal {
    def modifyS[St](initial: St, f: (St, A) => (St, B))(s: S): (St, T) = {
      traversal.modifyF[State[St, ?]]((a: A) => State(
        (state: St) => f(state, a)
      ))(s).run(initial)
    }
    def modifyS[St](f: (St, A) => (St, B))(s: (St, S)): (St, T) = {
      traversal.modifyF[State[St, ?]]((a: A) => State(
        (state: St) => f(state, a)
      ))(s._2).run(s._1)
    }
  }

  implicit final class TraversalModifyWithState[S, A](val traversal: Traversal[S, A]) extends AnyVal {
    def modifyS[St](initial: St, f: (St, A) => (St, A))(s: S): (St, S) = {
      traversal.modifyF[State[St, ?]]((a: A) => State(
        (state: St) => f(state, a)
      ))(s).run(initial)
    }
    def modifyS[St](f: (St, A) => (St, A))(s: (St, S)): (St, S) = {
      traversal.modifyF[State[St, ?]]((a: A) => State(
        (state: St) => f(state, a)
      ))(s._2).run(s._1)
    }
  }
}

