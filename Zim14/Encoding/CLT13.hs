{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Zim14.Encoding.CLT13 where

import Zim14.Circuit
import Zim14.Encoding
import Zim14.Index
import Zim14.Obfuscate (Obfuscation)
import Zim14.Util (i2b, b2i, red)

import CLT13.Rand
import qualified CLT13 as CLT

import Control.DeepSeq (NFData)
import Data.Monoid
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Text.Printf

newtype CLT13 = CLT13 {
    getCLT :: CLT.Encoding
} deriving (NFData, Generic, Serialize)

instance Show CLT13 where
    show _ = "[<clt13>]"

cltEncode :: CLT.MMap -> Indexer -> Integer -> Integer -> Index -> Rand CLT13
cltEncode = undefined

cltEval :: Obfuscation CLT13 -> CLT.PublicParams -> Circuit -> [Bool] -> Bool
cltEval = undefined
