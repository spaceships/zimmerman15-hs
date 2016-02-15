{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Zim14.Sym where

import Zim14.Util

import Control.DeepSeq
import Data.List.Split (splitOn)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- Sym is used as an index into a Map defining the obfuscation.
-- I use it to name the files corresponding to the obfuscation encodings,
-- so there was some boilerplate in order to show & read it properly
-- (without spaces). That's why it lives in its own module.

data Sym = X Int Bool
         | U Int Bool
         | Y Int
         | V
         | Z Int Bool
         | W Int Bool
         | C
         | S Int Int Bool Bool
         deriving (Eq, Ord, Generic, NFData, Serialize)

instance Show Sym where
    show (X i b) = "X-" ++ show i ++ "-" ++ show (b2i b)
    show (U i b) = "U-" ++ show i ++ "-" ++ show (b2i b)
    show (Y i)   = "Y-" ++ show i
    show V       = "V"
    show (Z i b) = "Z-" ++ show i ++ "-" ++ show (b2i b)
    show (W i b) = "W-" ++ show i ++ "-" ++ show (b2i b)
    show C       = "C"
    show (S i1 i2 b1 b2) = "S-" ++ show i1 ++ "-" ++ show i2 ++ "-"
                                ++ show (b2i b1) ++ "-" ++ show (b2i b2)

instance Read Sym where
    readsPrec _ s =
        let (x:rest) = splitOn "-" s
        in case x of
            "X" -> let [i,b] = rest in [(X (read i) (i2b (read b)), "")]
            "U" -> let [i,b] = rest in [(U (read i) (i2b (read b)), "")]
            "Y" -> let [i]   = rest in [(Y (read i), "")]
            "V" -> [(V, "")]
            "Z" -> let [i,b] = rest in [(Z (read i) (i2b (read b)), "")]
            "W" -> let [i,b] = rest in [(W (read i) (i2b (read b)), "")]
            "C" -> [(C, "")]
            "S" -> let [i1,i2,b1,b2] = rest
                   in [(S (read i1) (read i2) (i2b (read b1)) (i2b (read b2)), "")]
            _ -> error (show x)

