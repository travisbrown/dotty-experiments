/*package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Eq, Hash}

private[kernel] class TupleHasher[A <: Tuple] {
	type Out <: Tuple
	def apply(a: A): Out
	def hash(a: A): Int = apply(a).hashCode
}

private[kernel] object TupleHasher {
	given TupleHasher0[H] as TupleHasher[H *: Unit] given (H: Hash[H]) {
		type Out = Int *: Unit
		def apply(a: H *: Unit): Out = H.hash(a.head) *: ()
	}

	given TupleHasher1[H, T, TO] as TupleHasher[H *: T] given (H: Hash[H], T: TupleHasher[T] { type Out = TO }) {
		protected type Out = Int *: T
		protected def apply(a: H *: T): Out = a match {
			case h *: t => H.hash(h) *: T(t)
		}
	}
}

private[kernel] class TupleHash[H, T <: Tuple] given (H: Eq[H], T: Eq[T], TH: TupleHasher[H *: T]) extends Hash[H *: T] {

}*/