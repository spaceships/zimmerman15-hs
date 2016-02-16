{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Zim14.Obfuscate where

import Zim14.Circuit
import Zim14.Index
import Zim14.Sym
import Zim14.Util (b2i)

import CLT13.Rand
import CLT13.Util (pmap, forceM)
import qualified CLT13 as CLT

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Monoid
import Text.Printf
import qualified Data.Map.Strict as M

import Debug.Trace

type Obfuscation a = M.Map Sym a

data Params = Params {
    n     :: Int,
    m     :: Int,
    d     :: Int,
    λ     :: Int,
    n_ev  :: Integer,
    n_chk :: Integer
}

instance Show Params where
    show (Params {..}) =
        printf "Params: n=%d m=%d d=%d λ=%d n_ev=%d n_chk=%d" n m d λ n_ev n_chk

type N = (Integer, Integer) -- (N_ev, N_chk)

type Encoder a = Integer -> Integer -> Index -> Rand a

params :: Int -> Circuit -> IO Params
params λ c = do
    [n_chk, n_ev] <- randIO (randPrimes 2 λ)
    return $ Params {
        n     = ninputs c,
        m     = nconsts c,
        d     = depth c,
        λ     = λ,
        n_ev  = n_ev,
        n_chk = n_chk
    }

obfuscate :: NFData a => Bool -> Params -> Encoder a -> Circuit -> IO (Obfuscation a)
obfuscate verbose (Params {..}) encode c = do
    αs <- map fst <$> randIO (randInvs n n_chk)
    βs <- map fst <$> randIO (randInvs m n_chk)
    let c_val = evalMod c αs βs n_chk
    when verbose $ traceM (printf "top-level check val = %d" c_val)

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
                           xix = pow (X i (not b)) (xdegs c !! i)
                           wix = pow1 (W i)
                           bc  = bitCommit i b
                           zix = mconcat [xix, wix, bc]
                       in encode (δs !! i) (γs !! i) zix

        get (W_ i b) = let γs  = if b then γ1s else γ0s
                           bc  = bitCommit i b
                           wix = bc <> pow1 (W i)
                       in encode 0 (γs !! i) wix

        get C_ = let yix  = pow Y (ydeg c)
                     rest = mconcat $ do
                         (i, xdeg) <- zip [0..n-1] (xdegs c)
                         let xi0 = pow (X i False) xdeg
                             xi1 = pow (X i True)  xdeg
                             zix = pow1 (Z i)
                         return $ mconcat [xi0, xi1, zix]
                     cix = yix <> rest
                 in encode 0 c_val cix

        get (S_ i1 i2 b1 b2) | i1 == i2  = error "[get] S_ undefined when i1 = i2"
                             | i1 > i2   = get (S_ i2 i1 b1 b2)
                             | otherwise = encode 1 1 (bitFill i1 i2 b1 b2)

    m <- randIO (runGetter verbose n m get)
    return m

-- runGetter takes instructions how to generate each element, generates them,
-- them returns a big ol map of them
runGetter :: NFData a => Bool -> Int -> Int -> (Sym -> Rand a) -> Rand (M.Map Sym a)
runGetter verbose n m get = do
    let tr = if verbose then trace else flip const

    let g s = (s,) <$> get s

    let is = [0..n-1]
        js = [0..m-1]
        bs = [False, True]

    let xs = tr "generating xs" $ [ g (X_ i b) | i <- is, b <- bs ]
        us = tr "generating us" $ [ g (U_ i b) | i <- is, b <- bs ]
        ys = tr "generating ys" $ [ g (Y_ j)   | j <- js ]
        v  = tr "generating v"  $ g V_
        zs = tr "generating zs" $ [ g (Z_ i b) | i <- is, b <- bs ]
        ws = tr "generating ws" $ [ g (W_ i b) | i <- is, b <- bs ]
        c  = tr "generating c"  $ g C_
        ss = tr "generating ss" $ [ g (S_ i1 i2 b1 b2)
                                  | i1 <- is, i2 <- is
                                  , b1 <- bs, b2 <- bs, i1 /= i2
                                  ]

        actions = xs ++ us ++ ys ++ [v] ++ zs ++ ws ++ [c] ++ ss

    rngs <- splitRand (length actions)
    let res = pmap (uncurry evalRand) (zip actions rngs)
    return (M.fromList res)
