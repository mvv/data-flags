{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell utils for declaring flags instances.
module Data.Flags.TH (
    dataBitsAsFlags,
    dataBitsAsBoundedFlags,
    bitmaskWrapper
  ) where

import Language.Haskell.TH

import Data.Bits (Bits(..))
import Data.Maybe (isJust)
import Data.List (find, union, intercalate)
import Control.Applicative ((<$>))

import Data.Flags.Base

inst :: Name -> Name -> [Dec] -> Dec
inst className typeName = InstanceD [] (AppT (ConT className) (ConT typeName))

fun :: Name -> Exp -> Dec
fun name expr = FunD name [Clause [] (NormalB expr) []]

-- | Produces 'Data.Flags.Base.Flags' instance declaration for the specified
--   instance of 'Data.Bits.Bits'.
dataBitsAsFlags :: Name -> Q [Dec]
dataBitsAsFlags typeName = do
  noneE <- [| fromInteger 0 |]
  unionE <- [| (.|.) |]
  intersectionE <- [| (.&.) |] 
  differenceE <- [| \x -> \y -> x .&. (complement y) |]
  return [inst ''Flags typeName
            [fun 'noFlags noneE,
             fun 'andFlags unionE,
             fun 'commonFlags intersectionE,
             fun 'butFlags differenceE]]

-- | Produces 'Data.Flags.Base.Flags' and 'Data.Flags.Base.BoundedFlags'
--   instances declarations for the specified instance of 'Data.Bits.Bits'.
dataBitsAsBoundedFlags :: Name -> Q [Dec]
dataBitsAsBoundedFlags typeName = do
  allE <- [| fromInteger (-1) |]
  enumE <- [| \x -> map (setBit 0) $ filter (testBit x) [0 .. bitSize x - 1] |]
  (++ [inst ''BoundedFlags typeName
         [fun 'allFlags allE,
          fun 'enumFlags enumE]]) <$> dataBitsAsFlags typeName
  
bitmaskWrapper :: String -- ^ Wrapping type name
               -> Name -- ^ Wrapped type name
               -> [Name] -- ^ Types to derive automatically
               -> [(String, Integer)] -- ^ Individual flags
               -> Q [Dec]
bitmaskWrapper typeNameS wrappedName derives elems = do
  typeName <- return $ mkName typeNameS
  showE <- [| \flags -> $(stringE typeNameS) ++ " [" ++
                        (intercalate ", " $ map snd $
                           filter ((noFlags /=) . commonFlags flags . fst) $
                             $(listE $
                                 map (\(name, _) ->
                                        tupE [varE $ mkName name,
                                              stringE name])
                                     elems)) ++ "]" |]
  return $ [NewtypeD [] typeName []
                        (NormalC typeName [(NotStrict, ConT wrappedName)]) 
                        (union [''Eq, ''Flags] derives)] ++
           (concatMap (\(nameS, value) ->
                         let name = mkName nameS in 
                           [SigD name (ConT typeName),
                            FunD name
                              [Clause [] (NormalB $
                                            AppE (ConE typeName)
                                                 (LitE $ IntegerL value))
                                      []]]) elems) ++
           (if (isJust $ find (''Show ==) derives)
              then []
              else [inst ''Show typeName [fun 'show showE]])

