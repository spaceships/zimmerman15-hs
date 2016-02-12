module Zim14.Index where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

data FormalSymbol = SymY
                  | SymX Int Bool
                  | SymZ Int
                  | SymW Int
                  | SymS Int Bool Int
                  deriving (Show, Eq, Ord)

-- Index is the index of the z_i corresponding to a formal symbol
type Index = Int

-- Indexer turns formal symbols into Index
type Indexer = FormalSymbol -> [Index]

indexer :: Int -> Indexer
indexer n = ix
  where
    nys = 1
    nxs = 2*n
    nzs = n
    nws = n

    szero = nys + nxs + nzs + nws

    ix :: FormalSymbol -> [Index]
    ix SymY = [0]
    ix (SymX i b) = [2*i + (if b then 1 else 0) + nys]
    ix (SymZ i) = [i + nys + nxs]
    ix (SymW i) = [i + nys + nxs + nzs]

    ix (SymS i False j) = let z = i*(2*n-1) + szero
                          in if j == 0
                             then [z]
                             else [z + 2*j - 1, z + 2*j]

    ix (SymS i True j) = let z = i*(2*n-1) + szero
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
bitCommit ix i b = ix (SymS i b i)

bitFill :: Indexer -> Int -> Int -> Bool -> Bool -> [Index]
bitFill ix i1 i2 b1 b2
    | i1 < i2   = ix (SymS i1 b1 i2) ++ ix (SymS i2 b2 i1)
    | otherwise = error "[bitFill] i1 >= i2!"
