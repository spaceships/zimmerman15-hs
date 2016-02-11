module Zim14.IndexSet where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

type Index = Int

data Indexer = Indexer { yindex   :: Index
                       , xindex   :: Int -> Bool -> Index
                       , zindex   :: Int -> Index
                       , windex   :: Int -> Index
                       , sindex   :: Int -> Bool -> Int -> [Index]
                       , sindices :: Int -> [Index]
                       }

indexer :: Int -> (Indexer, Int)
indexer n = (ix, nsyms)
  where
    nsyms = nys + nxs + nzs + nws + nss
    nys = 1
    nxs = 2*n
    nzs = n
    nws = n
    nss = n*(2*n - 1)
    szero = nys + nxs + nzs + nws

    sindex i False j = let z = i*(2*n-1) + szero
                       in if j == 0
                             then [z]
                             else [z + 2*j - 1, z + 2*j]
    sindex i True j = let z = i*(2*n-1) + szero
                      in if j == n-1
                            then [z + 2*j]
                            else [z + 2*j, z + 2*j+1]

    sindices i = let start = i*(2*n-1) + szero
                 in [ start .. start + 2*n-2 ]

    ix = Indexer { yindex   = 0
                 , xindex   = \i b -> 2*i + (if b then 1 else 0) + nys
                 , zindex   = \i -> i + nys + nxs
                 , windex   = \i -> i + nys + nxs + nzs
                 , sindex   = sindex
                 , sindices = sindices
                 }

bitCommit :: Indexer -> Int -> Bool -> [Index]
bitCommit ix i b = sindex ix i b i

bitFill :: Indexer -> Int -> Int -> Bool -> Bool -> [Index]
bitFill ix i1 i2 b1 b2
    | i1 < i2   = sindex ix i1 b1 i2 ++ sindex ix i2 b2 i1
    | otherwise = error "[bitFill] i1 >= i2!"
