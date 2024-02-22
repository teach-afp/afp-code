Type-level programming and faking dependent types in Haskell
============================================================

- [`ExprTypeIndexed.hs`](ExprTypeIndexed.hs):
  Tagless interpretation by indexing expression by their value types.
  * GADTs.
  * Runtime type representation vial singletons.
  * Existential types.

- [`ExprTypeRep.hs`](ExprTypeRep.hs):
  * Use `Typeable` class for type representation.

- [`Vec.hs`](Vec.hs):
  * Data kinds (type-level natural numbers).
  * Type-level computation via type families.
  * Singleton types to connect values to their type-level twins.

- [`TreeNat.hs`](TreeNat.hs):
  * Ordered binary search trees for natural numbers.

- [`Tree.hs`](Tree.hs):
  * Generic ordered binary search trees for keys that have a type-level representation.
