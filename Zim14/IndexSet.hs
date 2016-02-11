module Zim14.IndexSet where

import CLT13.Types (IndexSet)

import qualified Data.Map as S

type Index = Int

data Indexer = Indexer { yindex :: Index
                       , xindex :: Int -> Int -> Index
                       , zindex :: Int -> Index
                       , windex :: Int -> Index
                       , sindex :: Int -> Int -> Int -> [Index]
                       }

makeIndexer :: Int -> (Indexer, Int)
makeIndexer n = (ix, nsyms)
  where
    nsyms = nys + nxs + nzs + nws + nss
    nys = 1
    nxs = 2*n
    nzs = n
    nws = n
    nss = n*(2*n - 1)

    zero = nys + nxs + nzs + nws

    sindex i 0 j = if j == 0
                    then [(2*n - 1)*i + zero]
                    else [(2*n - 1)*i + zero + 2*j - 1, (2*n-1)*i + zero + 2*j]

    sindex i 1 j = if j == n-1
                    then [(2*n - 1)*i + zero + 2*j]
                    else [(2*n - 1)*i + zero + 2*j, (2*n-1)*i + zero + 2*j + 1]

    ix = Indexer { yindex = 0
                 , xindex = \i b -> 2*i + b           + nys
                 , zindex = \i -> i                   + nys + nxs
                 , windex = \i -> i                   + nys + nxs + nzs
                 , sindex = sindex
                 }
