module Zim14.Index where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

data IndexSym = IxY
              | IxX Int Bool
              | IxZ Int
              | IxW Int
              | IxS Int Bool Int
              deriving (Show, Eq, Ord)

-- Index is the index of the z_i corresponding to a formal symbol
type Index = Int

-- Indexer turns formal symbols into Index
type Indexer = IndexSym -> [Index]

indexer :: Int -> Indexer
indexer n = ix
  where
    nys = 1
    nxs = 2*n
    nzs = n
    nws = n

    szero = nys + nxs + nzs + nws

    ix :: IndexSym -> [Index]
    ix IxY = [0]
    ix (IxX i b) = [2*i + (if b then 1 else 0) + nys]
    ix (IxZ i) = [i + nys + nxs]
    ix (IxW i) = [i + nys + nxs + nzs]

    ix (IxS i False j) = let z = i*(2*n-1) + szero
                         in if j == 0
                             then [z]
                             else [z + 2*j - 1, z + 2*j]

    ix (IxS i True j) = let z = i*(2*n-1) + szero
                        in if j == n-1
                            then [z + 2*j]
                            else [z + 2*j, z + 2*j+1]

yindex :: Int
yindex = 0

xindices :: Int -> [Index]
xindices n = [1 .. 2*n]

zindices :: Int -> [Index]
zindices n = let start = 1 + 2*n in [start .. start+n-1]

windices :: Int -> [Index]
windices n = let start = 1 + 3*n in [start .. start+n-1]

sindices :: Int -> [Index]
sindices n = [ start 0 .. start n - 1 ]
  where
    width   = 2*n-1
    start i = i*width + 4*n + 1

nindices :: Int -> Int
nindices n = n*(2*n-1) + 4*n + 1

bitCommit :: Indexer -> Int -> Bool -> [Index]
bitCommit ix i b = ix (IxS i b i)

bitFill :: Indexer -> Int -> Int -> Bool -> Bool -> [Index]
bitFill ix i1 i2 b1 b2
    | i1 < i2   = ix (IxS i1 b1 i2) ++ ix (IxS i2 b2 i1)
    | otherwise = error "[bitFill] i1 >= i2!"
