{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_ghc -fno-warn-orphans #-}

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
--   Or, using Template Haskell:
--
-- > import Data.Flags.TH
-- >
-- > $(bitmaskWrapper "MyFlags" ''CInt [] False
-- >     [("myFlag1", #{const C_FLAG1}),
-- >      ("myFlag2", #{const C_FLAG2}),
-- >      ("myFlag3", #{const C_FLAG3})])
-- >
--
--   And then use it like this:
--
-- > f $ myFlag1 .+. myFlag3
module Data.Flags
  ( Flags(..)
  , (.+.), (.-.), (.*.)

  , BoundedFlags(..)
  , allBut

  , containsAll, (.<=.), (.>=.)
  , containsSome, (.~.)
  , containsNone, (./~.)
  ) where

import Data.Bits ()
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Ptr (IntPtr, WordPtr)
import Foreign.C.Types (CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt,
                        CLong, CULong, CLLong, CULLong)

import Data.Flags.Base
import Data.Flags.TH

infixl 7 .*.
infixl 6 .+.
infixl 5 .-.
infix 4 .<=., .>=., `containsAll`, .~., `containsSome`, ./~., `containsNone`

-- | Alias for 'andFlags'.
(.+.) ∷ Flags α ⇒ α → α → α
(.+.) = andFlags

-- | Alias for 'butFlags'.
(.-.) ∷ Flags α ⇒ α → α → α
(.-.) = butFlags

-- | Alias for 'commonFlags'.
(.*.) ∷ Flags α ⇒ α → α → α
(.*.) = commonFlags

-- | Shorthand for 'allFlags' '.-.' /x/.
allBut ∷ BoundedFlags α ⇒ α → α
allBut = (allFlags .-.)

-- | Test if the first flag set contains all flags from the second.
containsAll ∷ Flags α ⇒ α → α → Bool
containsAll flags subflags = flags .*. subflags == subflags

-- | Alias for 'containsAll'.
(.>=.) ∷ Flags α ⇒ α → α → Bool
(.>=.) = containsAll

-- | Shorthand for 'flip' 'containsAll'.
(.<=.) ∷ Flags α ⇒ α → α → Bool
(.<=.) = flip containsAll

-- | Test if two flag sets intersect.
containsSome ∷ Flags α ⇒ α → α → Bool
containsSome flags subflags = flags .*. subflags /= noFlags

-- | Alias for 'containsSome'.
(.~.) ∷ Flags α ⇒ α → α → Bool
(.~.) = containsSome

-- | Test if two flag sets do not intersect.
containsNone ∷ Flags α ⇒ α → α → Bool
containsNone flags subflags = flags .*. subflags == noFlags

-- | Alias for 'containsNone'.
(./~.) ∷ Flags α ⇒ α → α → Bool
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
