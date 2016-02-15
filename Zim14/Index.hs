{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Zim14.Index where

import qualified CLT13 as CLT

import Control.DeepSeq (NFData)
import Data.Monoid
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M

data IndexSym = Y
              | X Int Bool
              | Z Int
              | W Int
              | S Int Bool Int
              deriving (Show, Eq, Ord, Generic, NFData)

type Power = Int
newtype Index = Index { getIndex :: M.Map IndexSym Power
                      } deriving (Eq, Ord, Generic, NFData)

instance Monoid Index where
    mappend a b = Index (M.unionWith (+) (getIndex a) (getIndex b))
    mempty      = Index M.empty

--------------------------------------------------------------------------------
-- bitcommit & bitfill from the paper

bitCommit :: Int -> Bool -> Index
bitCommit i b = pow1 (S i b i)

bitFill :: Int -> Int -> Bool -> Bool -> Index
bitFill i1 i2 b1 b2
    | i1 < i2   = pow1 (S i1 b1 i2) <> pow1 (S i2 b2 i1)
    | otherwise = error "[bitFill] i1 >= i2!"

--------------------------------------------------------------------------------
-- indexer: turn IndexSym into [CLT.Index]

-- CLT.Index is the index of the z_i corresponding to a formal symbol
-- Indexer turns a formal symbol into CLT.Index.
-- Indirection is needed since an IndexSym can be 1 *or more* CLT.Indices.
type Indexer = Index -> CLT.IndexSet

indexer :: Int -> Indexer
indexer n = indexer
  where
    nys = 1
    nxs = 2*n
    nzs = n
    nws = n
    szero = nys + nxs + nzs + nws

    indexer = CLT.indexUnions . map change . M.toList . getIndex
    change (sym, power) = CLT.pow (ix sym) power

    ix :: IndexSym -> [CLT.Index]
    ix Y = [0]
    ix (X i b) = [2*i + (if b then 1 else 0) + nys]
    ix (Z i) = [i + nys + nxs]
    ix (W i) = [i + nys + nxs + nzs]

    ix (S i False j) = let z = i*(2*n-1) + szero
                         in if j == 0
                             then [z]
                             else [z + 2*j - 1, z + 2*j]

    ix (S i True j) = let z = i*(2*n-1) + szero
                        in if j == n-1
                            then [z + 2*j]
                            else [z + 2*j, z + 2*j+1]

--------------------------------------------------------------------------------
-- functions to get listing of CLT.Indices based on n

yindex :: Int
yindex = 0

xindices :: Int -> [CLT.Index]
xindices n = [1 .. 2*n]

zindices :: Int -> [CLT.Index]
zindices n = let start = 1 + 2*n in [start .. start+n-1]

windices :: Int -> [CLT.Index]
windices n = let start = 1 + 3*n in [start .. start+n-1]

sindices :: Int -> [CLT.Index]
sindices n = [ start 0 .. start n - 1 ]
  where
    width   = 2*n-1
    start i = i*width + 4*n + 1

nindices :: Int -> Int
nindices n = n*(2*n-1) + 4*n + 1

--------------------------------------------------------------------------------
-- Index hepers

pow :: IndexSym -> Int -> Index
pow ix pow = Index $ M.singleton ix pow

pow1 :: IndexSym -> Index
pow1 = flip pow 1


-- Return the symbols from A that aren't in B, and their difference in Power.
-- Assumes that b is always smaller power than a
indexDiff :: Index -> Index -> Index
indexDiff a b = Index $ M.differenceWith f (getIndex a) (getIndex b)
  where
    f x y = let z = x - y in if z <= 0 then Nothing else Just z
