{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Zim14.Encoding.CLT13 where

import Zim14.Index
import Zim14.Util (i2b, b2i, red)
import Zim14.Encoding

import CLT13.Rand
import qualified CLT13 as CLT

import Control.DeepSeq (NFData)
import Data.Monoid
import GHC.Generics (Generic)
import Text.Printf

newtype CLT13 = CLT13 { getCLT :: CLT.Encoding }

instance Show CLT13 where
    show _ = "[<clt13>]"

cltEncode = undefined

cltEvalTest = undefined
