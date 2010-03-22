{-# LANGUAGE TemplateHaskell #-}

-- | This module provides type classes for working with sets of flags.
--   In particular, with wrappers around bit masks:
--
-- > import Data.Flags
-- >
-- > newtype MyFlags = MyFlags CInt deriving (Eq, Flags)
-- >
-- > #{enum MyFlags, MyFlags
-- >  , myFlag1 = C_FLAG1
-- >  , myFlag2 = C_FLAG2
-- >  , myFlag3 = C_FLAG3
-- >  }
-- >
-- > f :: MyFlags -> IO ()
-- > f = ...
--
--   And then use it like this:
--
-- > f $ myFlag1 .+. myFlag3
module Data.Flags (
    Flags(..),
    (.+.), (.-.), (.*.),

    BoundedFlags(..),
    allBut,

    containsAll, (.<=.), (.>=.),
    containsSome, (.~.),
    containsNone, (./~.),
  ) where

import Data.Bits ()
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Ptr (IntPtr, WordPtr)
import Foreign.C.Types (CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt,
                        CLong, CULong, CLLong, CULLong)

import Data.Flags.TH

infixl 8 .<=., .>=., `containsAll`, .~., `containsSome`, ./~., `containsNone`
infixl 7 .-., `butFlags`
infixl 6 .+., `andFlags`
infixl 5 .*., `commonFlags`

class Eq a => Flags a where
  -- | The empty set of flags.
  noFlags :: a
  -- | Union of two flag sets.
  andFlags :: a -> a -> a
  -- | Difference between two flag sets.
  butFlags :: a -> a -> a
  -- | Intersection of two flag sets.
  commonFlags :: a -> a -> a
  f1 `commonFlags` f2 = (f1 .+. f2) .-. (f1 .-. f2) .-. (f2 .-. f1)

-- | Alias for 'andFlags'.
(.+.) :: Flags a => a -> a -> a
(.+.) = andFlags

-- | Alias for 'butFlags'.
(.-.) :: Flags a => a -> a -> a
(.-.) = butFlags

-- | Alias for 'commonFlags'.
(.*.) :: Flags a => a -> a -> a
(.*.) = commonFlags

-- | Use this class when the set of flags is fixed and not likely
--   to change in the future.
class Flags a => BoundedFlags a where
  -- | Set of all flags.
  allFlags :: a
  -- | List the individual flags.
  enumFlags :: a -> [a]

-- | Shorthand for 'allFlags' '.-.' /x/.
allBut :: BoundedFlags a => a -> a
allBut = (allFlags .-.)

-- | Test if the first flag set contains all flags from the second.
containsAll :: Flags a => a -> a -> Bool
containsAll flags subflags = flags .*. subflags == subflags

-- | Alias for 'containsAll'.
(.>=.) :: Flags a => a -> a -> Bool
(.>=.) = containsAll

-- | Shorthand for 'flip' 'containsAll'.
(.<=.) :: Flags a => a -> a -> Bool
(.<=.) = flip containsAll

-- | Test if two flag sets intersect.
containsSome :: Flags a => a -> a -> Bool
containsSome flags subflags = flags .*. subflags /= noFlags

-- | Alias for 'containsSome'.
(.~.) :: Flags a => a -> a -> Bool
(.~.) = containsSome

-- | Test if two flag sets do not intersect.
containsNone :: Flags a => a -> a -> Bool
containsNone flags subflags = flags .*. subflags == noFlags

-- | Alias for 'containsNone'.
(./~.) :: Flags a => a -> a -> Bool
(./~.) = containsNone

$(dataBitsAsFlags ''Integer)
$(dataBitsAsBoundedFlags ''Int)
$(dataBitsAsBoundedFlags ''Int8)
$(dataBitsAsBoundedFlags ''Int16)
$(dataBitsAsBoundedFlags ''Int32)
$(dataBitsAsBoundedFlags ''Int64)
$(dataBitsAsBoundedFlags ''Word)
$(dataBitsAsBoundedFlags ''Word8)
$(dataBitsAsBoundedFlags ''Word16)
$(dataBitsAsBoundedFlags ''Word32)
$(dataBitsAsBoundedFlags ''Word64)
$(dataBitsAsBoundedFlags ''IntPtr)
$(dataBitsAsBoundedFlags ''WordPtr)
$(dataBitsAsBoundedFlags ''CChar)
$(dataBitsAsBoundedFlags ''CSChar)
$(dataBitsAsBoundedFlags ''CUChar)
$(dataBitsAsBoundedFlags ''CShort)
$(dataBitsAsBoundedFlags ''CUShort)
$(dataBitsAsBoundedFlags ''CInt)
$(dataBitsAsBoundedFlags ''CUInt)
$(dataBitsAsBoundedFlags ''CLong)
$(dataBitsAsBoundedFlags ''CULong)
$(dataBitsAsBoundedFlags ''CLLong)
$(dataBitsAsBoundedFlags ''CULLong)

