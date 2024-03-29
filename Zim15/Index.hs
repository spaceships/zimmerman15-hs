{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Zim15.Index where

import Zim15.Circuit
import Zim15.Util (b2i)

import qualified CLT13 as CLT

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict
import Data.Map ((!))
import Data.Monoid
import GHC.Generics (Generic)
import Text.Printf
import Data.Serialize (Serialize)
import qualified Data.Map.Strict as M

-- the trouble is that straddling sets map to symbols that have
-- more than one mapping in CLT. Then when we want to find the
-- top level encoding, we have to translate it to CLT. So
-- to get around this, we use a new index "F" for "Fresh",
-- that will be translated individually to a straddling set z
-- in CLT.

data IndexSym = Y
              | X Int Bool
              | Z Int
              | W Int
              | F Int Int -- F i j: The jth fresh var from the ith spanning set
              deriving (Eq, Ord, Generic, NFData, Serialize)

instance Show IndexSym where
    show Y           = "Y"
    show (X i b)     = printf "X_{%d,%d}" i (b2i b :: Int)
    show (Z i)       = printf "Z_%d" i
    show (W i)       = printf "W_%d" i
    show (F i j)     = printf "F_{%d,%d}" i j

type Power = Int

newtype Index = Index {
    getIndex :: M.Map IndexSym Power
} deriving (Eq, Ord, Generic, NFData, Serialize)

instance Show Index where
    show = unwords . map showElem . filter ((>0).snd) . M.toList . getIndex
      where
        showElem (i,p) = printf "%s^%d" (show i) p

instance Monoid Index where
    mappend a b = Index (M.unionWith (+) (getIndex a) (getIndex b))
    mempty      = Index M.empty

accumIndex :: (IndexSym -> a -> a) -> Index -> a -> a
accumIndex f ix x = foldr f x indices
  where
    indices = concatMap (\(isym,p) -> replicate p isym) $ M.toList (getIndex ix)

--------------------------------------------------------------------------------
-- mucking about with straddling sets

getS :: Int -> Int -> Bool -> Int -> [IndexSym]
getS _ i False j
    | j == 0    = [F i 0]
    | otherwise = [F i (2*j-1), F i (2*j)]
getS n i True j
    | j == n-1  = [F i (2*j)]
    | otherwise = [F i (2*j), F i (2*j+1)]

bitCommit :: Int -> Int -> Bool -> Index
bitCommit n i b = pow1 (getS n i b i)

bitFill :: Int -> Int -> Int -> Bool -> Bool -> Index
bitFill n i1 i2 b1 b2
    | i1 < i2   = pow1 (getS n i1 b1 i2) <> pow1 (getS n i2 b2 i1)
    | otherwise = error "[bitFill] i1 >= i2!"

--------------------------------------------------------------------------------
-- indexer: turn IndexSym into [CLT.Index]

-- CLT.Index is the index of the z_i corresponding to a formal symbol Indexer
-- turns a formal symbol into CLT.Index (as in "which z_i does this symbol
-- correspond to?")
type Indexer = Index -> CLT.IndexSet

indexer :: Circuit -> Indexer
indexer c = M.mapKeys (m!) . getIndex
  where
    tl = topLevelIndex c
    m  = fst $ flip execState (M.empty, 0) $ do
            let syms = M.keys (getIndex tl) :: [IndexSym]
            mapM_ assign syms
    assign :: IndexSym -> State (M.Map IndexSym CLT.Index, CLT.Index) ()
    assign sym = modify $ \(m', i) -> (M.insert sym i m', i + 1)

--------------------------------------------------------------------------------
-- functions to get listing of CLT.Indices based on n

numIndices :: Circuit -> Int
numIndices c = M.size $ getIndex $ topLevelIndex c

topLevelIndex :: Circuit -> Index
topLevelIndex c = mconcat [y, xs, zs, ws, ss]
  where
    n  = ninputs c
    y  = pow [Y] (ydeg c)
    xs = mconcat [ pow [X i b] (xdeg c i) | i <- [0..n-1], b <- [False, True] ]
    zs = pow1 [ Z i   | i <- [0..n-1] ]
    ws = pow1 [ W i   | i <- [0..n-1] ]
    ss = pow1 [ F i j | i <- [0..n-1], j <- [0..2*n-2] ]

topLevelCLTIndex :: Circuit -> CLT.IndexSet
topLevelCLTIndex c = indexer c (topLevelIndex c)

--------------------------------------------------------------------------------
-- Index hepers

pow :: [IndexSym] -> Int -> Index
pow ix p = Index $ M.fromList (map (, p) ix)

pow1 :: [IndexSym] -> Index
pow1 = flip pow 1

-- Return the symbols from A that aren't in B, and their difference in Power.
-- Assumes that b is always smaller power than a
indexMinus :: Index -> Index -> Index
indexMinus a b = Index $ filterZeroes $ M.differenceWith f (getIndex a) (getIndex b)
  where
    f x y = let z = x - y in if z <= 0 then Nothing else Just z
    filterZeroes = M.filter (> 0)

indexDiff :: Index -> Index -> Index
indexDiff a b = indexUnion (indexMinus a b) (indexMinus b a)

indexEq :: Index -> Index -> Bool
indexEq a b = M.null $ getIndex (indexDiff a b)

indexUnion :: Index -> Index -> Index
indexUnion a b = Index $ M.unionWith max (getIndex a) (getIndex b)
