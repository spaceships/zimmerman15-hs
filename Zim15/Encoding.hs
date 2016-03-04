{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Zim15.Encoding where

import Zim15.Index (Index)

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Text.Printf

data Encoding a = Encoding {
    ix  :: Index,
    val :: a
} deriving (Eq, Generic, NFData, Serialize)

instance Show a => Show (Encoding a) where
    show enc = printf "%s(%s)" (show (val enc)) (show (ix enc))
    {-show enc = show (val enc)-}
