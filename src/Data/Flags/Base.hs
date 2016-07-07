{-# LANGUAGE UnicodeSyntax #-}

module Data.Flags.Base
  ( Flags(..)
  , BoundedFlags(..)
  ) where

infixl 7 `commonFlags`
infixl 6 `andFlags`
infixl 5 `butFlags`

class Eq α ⇒ Flags α where
  -- | The empty set of flags.
  noFlags ∷ α
  -- | Union of two flag sets.
  andFlags ∷ α -> α -> α
  -- | Difference between two flag sets.
  butFlags ∷ α -> α -> α
  -- | Intersection of two flag sets.
  commonFlags ∷ α -> α -> α
  f1 `commonFlags` f2 =
    (f1 `andFlags` f2) `butFlags` (f1 `butFlags` f2) `butFlags`
    (f2 `butFlags` f1)

-- | Use this class when the set of flags is fixed and not likely
--   to change in the future.
class Flags α ⇒ BoundedFlags α where
  -- | Set of all flags.
  allFlags ∷ α
  -- | List the individual flags.
  enumFlags ∷ α -> [α]
