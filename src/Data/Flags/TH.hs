{-# LANGUAGE TemplateHaskell #-}

module Data.Flags.TH (
    dataBitsAsFlags,
    dataBitsAsBoundedFlags
  ) where

import Language.Haskell.TH

import Data.Bits (Bits(..))
import Control.Applicative ((<$>))

inst :: String -> Name -> [Dec] -> Dec
inst name typeName = InstanceD [] (AppT (ConT $ mkName name) (ConT typeName))

fun :: String -> Exp -> Dec
fun name expr = FunD (mkName name) [Clause [] (NormalB expr) []]

dataBitsAsFlags :: Name -> Q [Dec]
dataBitsAsFlags typeName = do
  noneE <- [| fromInteger 0 |]
  unionE <- [| (.|.) |]
  intersectionE <- [| (.&.) |] 
  differenceE <- [| \x -> \y -> x .&. (complement y) |]
  return [inst "Flags" typeName
            [fun "noFlags" noneE,
             fun "andFlags" unionE,
             fun "commonFlags" intersectionE,
             fun "butFlags" differenceE]]

dataBitsAsBoundedFlags :: Name -> Q [Dec]
dataBitsAsBoundedFlags typeName = do
  allE <- [| fromInteger (-1) |]
  enumE <- [| \x -> map (setBit 0) $ filter (testBit x) [0 .. bitSize x - 1] |]
  (++ [inst "BoundedFlags" typeName
         [fun "allFlags" allE,
          fun "enumFlags" enumE]]) <$> dataBitsAsFlags typeName
  
