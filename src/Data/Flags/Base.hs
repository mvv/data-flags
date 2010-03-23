module Data.Flags.Base (
    Flags(..),
    BoundedFlags(..),
  ) where

infixl 7 `butFlags`
infixl 6 `andFlags`
infixl 5 `commonFlags`

class Eq a => Flags a where
  -- | The empty set of flags.
  noFlags :: a
  -- | Union of two flag sets.
  andFlags :: a -> a -> a
  -- | Difference between two flag sets.
  butFlags :: a -> a -> a
  -- | Intersection of two flag sets.
  commonFlags :: a -> a -> a
  f1 `commonFlags` f2 =
    (f1 `andFlags` f2) `butFlags` (f1 `butFlags` f2) `butFlags`
    (f2 `butFlags` f1)

-- | Use this class when the set of flags is fixed and not likely
--   to change in the future.
class Flags a => BoundedFlags a where
  -- | Set of all flags.
  allFlags :: a
  -- | List the individual flags.
  enumFlags :: a -> [a]

