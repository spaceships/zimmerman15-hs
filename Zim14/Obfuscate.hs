{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module Zim14.Obfuscate where

import Zim14.Circuit
import Zim14.IndexSet

import CLT13 (IndexSet)
import CLT13.Util (pmap)
import Data.List (nub)
import qualified Data.Map as M

data Obfuscation = Obfuscation { circ     :: Circuit
                               {-, topIndex :: IndexSet-}
                               {-, nu       :: Int-}
                               {-, q        :: Integer-}
                               {-, x0s      :: [Integer]-}
                               {-, x1s      :: [Integer]-}
                               }

obfuscate :: Int -> Circuit -> IO Obfuscation
obfuscate lambda c = do
    let d    = depth c
        n    = ninputs c
        ydeg = degree c (Const (-1) (-1))
        xdeg = pmap (degree c . Input) [0..n-1]
        tlix = topLevelIndex c ydeg xdeg
    print tlix
    return $ Obfuscation c

topLevelIndex :: Circuit -> Int -> [Int] -> IndexSet
topLevelIndex c ydeg xdegs = M.fromList (yix : xixs ++ zixs ++ wixs ++ sixs)
  where
    n = ninputs c
    (ix, nzs) = indexer n

    yix  = (yindex ix, ydeg)
    xixs = [(xindex ix i b, d) | i <- [0..n-1]
                               | d <- xdegs
                               , b <- [False, True]
                               ]
    zixs = [ (zindex ix i, 1) | i <- [0..n-1] ]
    wixs = [ (windex ix i, 1) | i <- [0..n-1] ]
    sixs = map (,1) $ concat [sindices ix i | i <- [0..n-1]]
