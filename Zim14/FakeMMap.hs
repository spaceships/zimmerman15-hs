{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Zim14.FakeMMap where

import Zim14.Index

import CLT13.IndexSet
import CLT13.Rand

import Control.DeepSeq
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.Map as M

data FakeEncoding = FakeEncoding { ev  :: Integer
                                 , chk :: Integer
                                 , ix  :: IndexSet
                                 } deriving (Serialize, Eq, Generic, NFData)

fakeEncode :: Integer -> Integer -> IndexSet -> Rand FakeEncoding
fakeEncode x y ix = return $ FakeEncoding x y ix

{-data ElemOps = ElemOps { add :: Element -> Element -> Element-}
                       {-, sub :: Element -> Element -> Element-}
                       {-, mul :: Element -> Element -> Element-}
                       {-}-}

{-mkOps :: Integer -> Integer -> ElemOps-}
{-mkOps n_ev n_chk = ElemOps add sub mul-}
  {-where-}
    {-add x y | ix x /= ix y = error "[add] unequal indices!"-}
    {-add x y = Element (ev x + ev y `mod` n_ev)-}
                      {-(chk x + chk y `mod` n_chk)-}
                      {-(ix x)-}
