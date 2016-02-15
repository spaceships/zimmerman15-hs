{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Zim14.Sym where

import Zim14.Util

import Control.DeepSeq
import Data.List.Split (splitOn)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- Sym represents an actual encoding.
-- Each encoding has an Index (the subscript).
-- The Index corresponds to an IndexSet in CLT.
-- Sym is used as an index into a Map defining the obfuscation.
-- I use it to name the files corresponding to the obfuscation encodings,
-- so there was some boilerplate in order to show & read it properly
-- (without spaces). That's why it lives in its own module.

data Sym = X_ Int Bool
         | U_ Int Bool
         | Y_ Int
         | V_
         | Z_ Int Bool
         | W_ Int Bool
         | C_
         | S_ Int Int Bool Bool
         deriving (Eq, Ord, Generic, NFData, Serialize)

instance Show Sym where
    show (X_ i b) = "X-" ++ show i ++ "-" ++ show (b2i b)
    show (U_ i b) = "U-" ++ show i ++ "-" ++ show (b2i b)
    show (Y_ i)   = "Y-" ++ show i
    show V_       = "V"
    show (Z_ i b) = "Z-" ++ show i ++ "-" ++ show (b2i b)
    show (W_ i b) = "W-" ++ show i ++ "-" ++ show (b2i b)
    show C_       = "C"
    show (S_ i1 i2 b1 b2) = "S-" ++ show i1 ++ "-" ++ show i2 ++ "-"
                                ++ show (b2i b1) ++ "-" ++ show (b2i b2)

instance Read Sym where
    readsPrec _ s =
        let (x:rest) = splitOn "-" s
        in case x of
            "X" -> let [i,b] = rest in [(X_ (read i) (i2b (read b)), "")]
            "U" -> let [i,b] = rest in [(U_ (read i) (i2b (read b)), "")]
            "Y" -> let [i]   = rest in [(Y_ (read i), "")]
            "V" -> [(V_, "")]
            "Z" -> let [i,b] = rest in [(Z_ (read i) (i2b (read b)), "")]
            "W" -> let [i,b] = rest in [(W_ (read i) (i2b (read b)), "")]
            "C" -> [(C_, "")]
            "S" -> let [i1,i2,b1,b2] = rest
                   in [(S_ (read i1) (read i2) (i2b (read b1)) (i2b (read b2)), "")]
            _   -> error (show x)
