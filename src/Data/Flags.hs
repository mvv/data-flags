{-# LANGUAGE TemplateHaskell #-}

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
  noFlags :: a
  andFlags :: a -> a -> a
  butFlags :: a -> a -> a
  commonFlags :: a -> a -> a
  f1 `commonFlags` f2 = (f1 .+. f2) .-. (f1 .-. f2) .-. (f2 .-. f1)

(.+.) :: Flags a => a -> a -> a
(.+.) = andFlags

(.-.) :: Flags a => a -> a -> a
(.-.) = butFlags

(.*.) :: Flags a => a -> a -> a
(.*.) = commonFlags

class Flags a => BoundedFlags a where
  allFlags :: a
  enumFlags :: a -> [a]

allBut :: BoundedFlags a => a -> a
allBut = (allFlags .-.)

containsAll :: Flags a => a -> a -> Bool
containsAll flags subflags = flags .*. subflags == subflags

(.>=.) :: Flags a => a -> a -> Bool
(.>=.) = containsAll

(.<=.) :: Flags a => a -> a -> Bool
(.<=.) = flip containsAll

containsSome :: Flags a => a -> a -> Bool
containsSome flags subflags = flags .*. subflags /= noFlags

(.~.) :: Flags a => a -> a -> Bool
(.~.) = containsSome

containsNone :: Flags a => a -> a -> Bool
containsNone flags subflags = flags .*. subflags == noFlags

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

