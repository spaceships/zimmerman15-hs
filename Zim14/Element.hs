module Zim14.Element where

import Zim14.Index

import CLT13.Types (IndexSet)
import qualified Data.Map as M

data Element = Element { ev  :: Integer
                       , chk :: Integer
                       , ix  :: IndexSet
                       }

encode :: Integer -> Integer -> IndexSet -> Element
encode = Element

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
