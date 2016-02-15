{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Zim14.Obfuscate where

import Zim14.Circuit
import Zim14.Sym
import Zim14.Util (b2i)
import Zim14.Index

import CLT13.Rand
import CLT13.Util (pmap, forceM)
import qualified CLT13 as CLT

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Monoid
import qualified Data.Map.Strict as M

type Obfuscation a = M.Map Sym a

data ObfParams = ObfParams { n     :: Int
                           , m     :: Int
                           , d     :: Int
                           , ix    :: Indexer
                           , nzs   :: Int
                           , ydeg  :: Int
                           , xdegs :: [Int]
                           , pows  :: CLT.IndexSet
                           }

type Encoder a = Integer -> Integer -> Index -> Rand a

obfParams :: Circuit -> ObfParams
obfParams c = ObfParams n m d ix nzs ydeg xdegs pows
  where
    d = depth c
    n = ninputs c
    m = nconsts c
    ix    = indexer n
    nzs   = nindices n
    ydeg  = degree c (outRef c) (Const (-1))
    xdegs = pmap (degree c (outRef c) . Input) [0..n-1]
    pows  = topLevelCLTIndex ix n ydeg xdegs

obfuscate :: NFData a => ObfParams -> Encoder a -> Int -> Circuit -> IO (Obfuscation a)
obfuscate (ObfParams {..}) encode λ c = do
    n_chk <- randIO (randInteger λ)
    n_ev  <- randIO (randInteger λ)

    αs <- map fst <$> randIO (randInvs n n_chk)
    βs <- map fst <$> randIO (randInvs m n_chk)
    let c_val = evalMod c αs βs n_chk

    γ0s <- map fst <$> randIO (randInvs n n_chk)
    γ1s <- map fst <$> randIO (randInvs n n_chk)

    δ0s <- map fst <$> randIO (randInvs n n_ev)
    δ1s <- map fst <$> randIO (randInvs n n_ev)

    -- get tells how to derive each element, and sequenceGetter actually does it
    let get (X_ i b) = encode (b2i b) (αs !! i) (pow1 (X i b))

        get (U_ i b) = encode 1 1 (pow1 (X i b))

        get (Y_ j)   = encode (consts c !! j) (βs !! j) (pow1 Y)

        get V_       = encode 1 1 (pow1 Y)

        get (Z_ i b) = let (δs, γs) = if b then (δ1s, γ1s) else (δ0s, γ0s)
                           xix = pow (X i (not b)) (xdegs !! i)
                           wix = pow1 (W i)
                           bc  = bitCommit i b
                           zix = mconcat [xix, wix, bc]
                       in encode (δs !! i) (γs !! i) zix

        get (W_ i b) = let γs  = if b then γ1s else γ0s
                           bc  = bitCommit i b
                           wix = bc <> pow1 (W i)
                       in encode 0 (γs !! i) wix

        get C_ = let yix  = pow Y ydeg
                     rest = mconcat $ do
                         (i, xdeg) <- zip [0..n-1] xdegs
                         let xi0 = pow (X i False) xdeg
                             xi1 = pow (X i True)  xdeg
                             zix = pow1 (Z i)
                         return $ mconcat [xi0, xi1, zix]
                     cix = yix <> rest
                 in encode 0 c_val cix

        get (S_ i1 i2 b1 b2) | i1 == i2  = error "[get] S_ undefined when i1 = i2"
                             | i1 > i2   = get (S_ i2 i1 b1 b2)
                             | otherwise = encode 1 1 (bitFill i1 i2 b1 b2)

    m <- randIO (runGetter n m get)
    return m

-- runGetter takes instructions how to generate each element, generates them,
-- them returns a big ol map of them
runGetter :: NFData a => Int -> Int -> (Sym -> Rand a) -> Rand (M.Map Sym a)
runGetter n m get = do
    let g s = (s,) <$> get s

    let is = [0..n-1]
        js = [0..m-1]
        bs = [False, True]

    let xs = [ g (X_ i b) | i <- is, b <- bs ]
        us = [ g (U_ i b) | i <- is, b <- bs ]
        ys = [ g (Y_ j)   | j <- js ]
        v  = g V_
        zs = [ g (Z_ i b) | i <- is, b <- bs ]
        ws = [ g (W_ i b) | i <- is, b <- bs ]
        c  = g C_
        ss = [ g (S_ i1 i2 b1 b2) | i1 <- is, i2 <- is, b1 <- bs, b2 <- bs, i1 /= i2 ]

        actions = xs ++ us ++ ys ++ [v] ++ zs ++ ws ++ [c] ++ ss

    rngs <- splitRand (length actions)
    let res = pmap (uncurry evalRand) (zip actions rngs)
    return (M.fromList res)

topLevelCLTIndex :: Indexer -> Int -> Int -> [Int] -> CLT.IndexSet
topLevelCLTIndex ix n ydeg xdegs = CLT.indexUnions (yix : xixs ++ zixs ++ wixs ++ sixs)
  where
    yix  = ix (pow Y ydeg)
    xixs = [ix (pow (X i b) d) | i <- [0..n-1] | d <- xdegs , b <- [False, True]]
    zixs = [ix (pow (Z i)   1) | i <- [0..n-1]]
    wixs = [ix (pow (W i)   1) | i <- [0..n-1]]
    sixs = map CLT.pow1 [sindices n]
