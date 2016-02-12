{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module Zim14.Obfuscate where

import Zim14.Circuit
import Zim14.Index
import Zim14.Element -- Faker module

import CLT13 (IndexSet)
import CLT13.Util (pmap, forceM)
import CLT13.Rand
import qualified CLT13 as CLT
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
    let d         = depth c
        n         = ninputs c
        m         = nconsts c
        (ix, nzs) = indexer n
        ydeg = degree c (Const (-1))
        xdeg = pmap (degree c . Input) [0..n-1]
        pows = topLevelIndex ix n ydeg xdeg

    {-mmap <- CLT.setup lambda d nzs pows-}
    n_chk <- randIO (randInteger lambda)
    n_ev  <- randIO (randInteger lambda)

    alphas  <- map fst <$> randIO (randInvs n n_chk)
    betas   <- map fst <$> randIO (randInvs m n_chk)
    let cstar = evalMod c alphas betas n_chk
    print cstar

    let (x0s, x1s) = encodeXs ix alphas
        ys = encodeYs ix (consts c) betas

    {-gamma0s <- map fst <$> randIO (randInvs n n_chk)-}
    {-gamma1s <- map fst <$> randIO (randInvs n n_chk)-}

    {-delta0s <- map fst <$> randIO (randInvs n n_ev)-}
    {-delta1s <- map fst <$> randIO (randInvs n n_ev)-}

    return $ Obfuscation c

encodeXs :: Indexer -> [Integer] -> ([Element], [Element])
encodeXs ix alphas = (x0s, x1s)
  where
    n    = length alphas
    ix0s = [M.singleton (xindex ix i False) 1 | i <- [0..n-1]]
    ix1s = [M.singleton (xindex ix i True)  1 | i <- [0..n-1]]
    x0s  = [encode 0 chk ix | chk <- alphas | ix <- ix0s]
    x1s  = [encode 1 chk ix | chk <- alphas | ix <- ix1s]

encodeYs :: Indexer -> [Integer] -> [Integer] -> [Element]
encodeYs ix ys betas = [encode y chk yix | y <- ys | chk <- betas]
  where
    yix = M.singleton (yindex ix) 1

topLevelIndex :: Indexer -> Int -> Int -> [Int] -> IndexSet
topLevelIndex ix n ydeg xdegs = M.fromList (yix : xixs ++ zixs ++ wixs ++ sixs)
  where
    yix  = (yindex ix, ydeg)
    xixs = [(xindex ix i b, d) | i <- [0..n-1]
                               | d <- xdegs
                               , b <- [False, True]
                               ]
    zixs = [ (zindex ix i, 1) | i <- [0..n-1] ]
    wixs = [ (windex ix i, 1) | i <- [0..n-1] ]
    sixs = map (,1) $ concat [sindices ix i | i <- [0..n-1]]
