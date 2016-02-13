{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Zim14.Obfuscate where

import Zim14.Circuit
import Zim14.Index

import CLT13.IndexSet
import CLT13.Util (pmap, plist, forceM)
import CLT13.Rand
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified CLT13 as CLT
import qualified Control.Monad.Parallel as P
import qualified Data.Map as M

import qualified Zim14.Element as E-- Faker module

data Sym = X Int Bool
         | U Int Bool
         | Y Int
         | V
         | Z Int Bool
         | W Int Bool
         | C
         | S Int Int Bool Bool
         deriving (Show, Eq, Ord, Generic, NFData)

data Obfuscation = Obfuscation { circ :: Circuit
                               , get  :: M.Map Sym Element
                               }

{-type Element = CLT.Encoding-}
type Element = E.Element

obfuscate :: Int -> Circuit -> IO Obfuscation
obfuscate λ c = do
    let d = depth c
        n = ninputs c
        m = nconsts c
        ix    = indexer n
        nzs   = nindices n
        ydeg  = degree c (Const (-1))
        xdegs = pmap (degree c . Input) [0..n-1]
        pows  = topLevelIndex ix n ydeg xdegs

    {-mmap <- CLT.setup λ d nzs pows-}
    {-let encode x y ix = CLT.encode [x,y] ix mmap-}
    let encode x y ix = return (E.encode x y ix)

    n_chk <- randIO (randInteger λ)
    n_ev  <- randIO (randInteger λ)

    αs <- map fst <$> randIO (randInvs n n_chk)
    βs <- map fst <$> randIO (randInvs m n_chk)
    let c_val = evalMod c αs βs n_chk
    print c_val

    γ0s <- map fst <$> randIO (randInvs n n_chk)
    γ1s <- map fst <$> randIO (randInvs n n_chk)

    δ0s <- map fst <$> randIO (randInvs n n_ev)
    δ1s <- map fst <$> randIO (randInvs n n_ev)

    -- get tells how to derive each element, and sequenceGetter actually does it
    let get :: Sym -> Rand Element
        get (X i b) = encode (b2i b) (αs !! i) (pow1 (ix (IxX i b)))

        get (U i b) = encode 1 1 (pow1 (ix (IxX i b)))

        get (Y j)   = encode (consts c !! j) (βs !! j) (pow1 (ix IxY))

        get V       = encode 1 1 (pow1 (ix IxY))

        get (Z i b) = let (δs, γs) = if b then (δ1s, γ1s) else (δ0s, γ0s)
                          xix = pow (ix (IxX i (not b))) (xdegs !! i)
                          wix = pow1 $ ix (IxW i)
                          bc  = pow1 (bitCommit ix i b)
                          zix = indexUnions [xix, wix, bc]
                      in encode (δs !! i) (γs !! i) zix

        get (W i b) = let γs  = if b then γ1s else γ0s
                          bc  = pow1 (bitCommit ix i b)
                          wix = indexUnion bc $ pow1 (ix (IxW i))
                      in encode 0 (γs !! i) wix

        get C = let yix  = pow (ix IxY) ydeg
                    rest = indexUnions $ do
                        (i, xdeg) <- zip [0..n-1] xdegs
                        let xi0 = pow (ix (IxX i False)) xdeg
                            xi1 = pow (ix (IxX i True))  xdeg
                            zix = pow1 (ix (IxZ i))
                        return $ indexUnions [xi0, xi1, zix]
                    cix = indexUnion yix rest
                in encode 0 c_val cix

        get (S i1 i2 b1 b2) | i1 == i2  = error "[get] S undefined when i1 = i2"
                            | i1 > i2   = get (S i2 i1 b1 b2)
                            | otherwise = encode 1 1 (pow1 (bitFill ix i1 i2 b1 b2))

    m <- randIO (runGetter n m get)
    putStrLn "encode stuff"
    forceM m
    return $ Obfuscation c m

-- runGetter takes instructions how to generate each element, generates them,
-- them returns a big ol map of them
runGetter :: Int -> Int -> (Sym -> Rand Element) -> Rand (M.Map Sym Element)
runGetter n m get = do
    let g s = (s,) <$> get s

    let is = [0..n-1]
        js = [0..m-1]
        bs = [False, True]

    let xs = [ g (X i b) | i <- is, b <- bs ]
        us = [ g (U i b) | i <- is, b <- bs ]
        ys = [ g (Y j)   | j <- js ]
        v  = g V
        zs = [ g (Z i b) | i <- is, b <- bs ]
        ws = [ g (W i b) | i <- is, b <- bs ]
        c  = g C
        ss = [ g (S i1 i2 b1 b2) | i1 <- is, i2 <- is, b1 <- bs, b2 <- bs, i1 /= i2 ]

        actions = xs ++ us ++ ys ++ [v] ++ zs ++ ws ++ [c] ++ ss :: [Rand (Sym, Element)]

    rngs <- splitRand (length actions)
    let res = pmap (uncurry evalRand) (zip actions rngs) :: [(Sym, Element)]
    return (M.fromList res)

topLevelIndex :: Indexer -> Int -> Int -> [Int] -> IndexSet
topLevelIndex ix n ydeg xdegs = indexUnions (yix : xixs ++ zixs ++ wixs ++ sixs)
  where
    yix  = pow (ix IxY) ydeg
    xixs = [pow (ix (IxX i b)) d | i <- [0..n-1]
                                  | d <- xdegs
                                  , b <- [False, True]
                                  ]
    zixs = [ pow1 (ix (IxZ i)) | i <- [0..n-1] ]
    wixs = [ pow1 (ix (IxW i)) | i <- [0..n-1] ]
    sixs = map pow1 [sindices n]

b2i :: Bool -> Integer
b2i False = 0
b2i True  = 1
