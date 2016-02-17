{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Zim14.Encoding.CLT13 where

import Zim14.Circuit
import Zim14.Evaluate
import Zim14.Index
import Zim14.Obfuscate (Obfuscation, Encoder)

import qualified CLT13 as CLT

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

newtype CLT13 = CLT13 {
    getCLT :: CLT.Encoding
} deriving (NFData, Generic, Serialize)

instance Show CLT13 where
    show _ = "[<clt13>]"

-- cltEncode :: CLT.MMap -> Indexer -> (Integer -> Integer -> Index -> Rand CLT13)
cltEncode :: CLT.MMap -> Indexer -> Encoder CLT13
cltEncode mmap ixr x y ix = CLT13 <$> CLT.encode [x,y] (ixr ix) mmap

cltEval :: Obfuscation CLT13 -> CLT.PublicParams -> Circuit -> [Bool] -> Bool
cltEval obf pp c xs = not $ CLT.isZero pp (getCLT res)
  where
    res = eval cltEv obf c xs

    cltEv = ObfEvaluator {
        evMul = cltMul pp,
        evAdd = cltAdd pp,
        evSub = cltSub pp
    }

cltMul :: CLT.PublicParams -> CLT13 -> CLT13 -> CLT13
cltMul pp x y = CLT13 $ CLT.mul pp (getCLT x) (getCLT y)

cltAdd :: CLT.PublicParams -> CLT13 -> CLT13 -> CLT13
cltAdd pp x y = CLT13 $ CLT.add pp (getCLT x) (getCLT y)

cltSub :: CLT.PublicParams -> CLT13 -> CLT13 -> CLT13
cltSub pp x y = CLT13 $ CLT.sub pp (getCLT x) (getCLT y)
