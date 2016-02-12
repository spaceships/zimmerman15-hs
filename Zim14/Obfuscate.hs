{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module Zim14.Obfuscate where

import Zim14.Circuit
import Zim14.Index
import Zim14.Element -- Faker module

import CLT13.IndexSet
import CLT13.Util (pmap, forceM)
import CLT13.Rand
import Data.List (zip4)
import qualified CLT13 as CLT
import qualified Data.Map as M

data Sym = X Int Bool
         | U Int Bool
         | Y Int
         | V
         | Z Int Bool
         | W Int Bool
         | C
         | S Int Int Bool Bool
         deriving (Show, Eq, Ord)

data Obfuscation = Obfuscation { circ :: Circuit
                               , get  :: Sym -> Element
                               }

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

    {-mmap <- CLT.setup lambda d nzs pows-}
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

    let (x0s, x1s) = encodeXs ix n αs
        (u0s, u1s) = encodeUs ix n
        ys         = encodeYs ix (consts c) βs
        v          = encodeV  ix
        (z0s, z1s) = encodeZs ix n xdegs (δ0s, δ1s) (γ0s, γ1s)
        (w0s, w1s) = encodeWs ix n (γ0s, γ1s)

    let get (X i b) = return $ encode (b2i b) (αs !! i) (pow1 (ix (IxX i b)))

        get (U i b) = return $ encode 1 1 (pow1 (ix (IxX i b)))

        get (Y i)   = return $ encode (consts c !! i) (βs !! i) (pow1 (ix IxY))

        get V       = return $ encode 1 1 (pow1 (ix IxY))

        get (Z i b) = let (δs, γs) = if b then (δ1s, γ1s) else (δ0s, γ0s)
                          xix = pow (ix (IxX i (not b))) (xdegs !! i)
                          wix = pow1 $ ix (IxW i)
                          bc  = pow1 (bitCommit ix i b)
                          zix = indexUnions [xix, wix, bc]
                      in return $ encode (δs !! i) (γs !! i) zix

        get (W i b) = let γs  = if b then γ1s else γ0s
                          bc  = pow1 (bitCommit ix i b)
                          wix = indexUnion bc $ pow1 (ix (IxW i))
                      in return $ encode 0 (γs !! i) wix

        get C = let yix  = pow (ix IxY) ydeg
                    rest = indexUnions $ do
                        (i, xdeg) <- zip [0..n-1] xdegs
                        let xi0 = pow (ix (IxX i False)) xdeg
                            xi1 = pow (ix (IxX i True))  xdeg
                            zix = pow1 (ix (IxZ i))
                        return $ indexUnions [xi0, xi1, zix]
                    cix = indexUnion yix rest
                in return $ encode 0 c_val cix

        get (S i1 i2 b1 b2) | i1 == i2  = error "[get] S undefined when i1 = i2"
                            | i1 > i2   = get (S i2 i1 b1 b2)
                            | otherwise = return $ encode 1 1 (pow1 (bitFill ix i1 i2 b1 b2))

    return $ Obfuscation c undefined

encodeXs :: Indexer -> Int -> [Integer] -> ([Element], [Element])
encodeXs ix n αs = (x0s, x1s)
  where
    ix0s = [pow1 (ix (IxX i False)) | i <- [0..n-1]]
    ix1s = [pow1 (ix (IxX i True))  | i <- [0..n-1]]
    x0s  = [encode 0 chk ix | chk <- αs | ix <- ix0s]
    x1s  = [encode 1 chk ix | chk <- αs | ix <- ix1s]

encodeUs :: Indexer -> Int -> ([Element], [Element])
encodeUs ix n = (u0s, u1s)
  where
    u0s = [encode 1 1 (pow1 (ix (IxX i False))) | i <- [0..n-1]]
    u1s = [encode 1 1 (pow1 (ix (IxX i True)))  | i <- [0..n-1]]

encodeYs :: Indexer -> [Integer] -> [Integer] -> [Element]
encodeYs ix ys βs = [encode y chk (pow1 (ix IxY)) | y <- ys | chk <- βs]

encodeV :: Indexer -> Element
encodeV ix = encode 1 1 (pow1 (ix IxY))

encodeZs :: Indexer -> Int -> [Int]
         -> ([Integer], [Integer])
         -> ([Integer], [Integer])
         -> ([Element], [Element])
encodeZs ix n xdegs (δ0s, δ1s) (γ0s, γ1s) = (zs False, zs True)
  where
    zs b = do
        let (δs, γs) = if b then (δ1s, γ1s) else (δ0s, γ0s)
        (i, xdeg, δ, γ) <- zip4 [0..n-1] xdegs δs γs
        let xix = pow (ix (IxX i (not b))) xdeg
            wix = pow1 (ix (IxW i))
            bc  = pow1 (bitCommit ix i b)
            zix = indexUnions [xix, wix, bc]
        return (encode δ γ zix)

encodeWs :: Indexer -> Int -> ([Integer], [Integer]) -> ([Element], [Element])
encodeWs ix n (γ0s, γ1s) = (ws False, ws True)
  where
    ws b = do
        let γs = if b then γ1s else γ0s
        (i, γ) <- zip [0..n-1] γs
        let bc  = pow1 (bitCommit ix i b)
            wix = indexUnion bc (pow1 (ix (IxW i)))
        return (encode 0 γ wix)

encodeCStar :: Indexer -> Int -> [Int] -> Int -> Integer -> Element
encodeCStar ix n xdegs ydeg cstar_val = encode 0 cstar_val cix
  where
    yix  = pow (ix IxY) ydeg
    rest = indexUnions $ do
        (i, xdeg) <- zip [0..n-1] xdegs
        let xi0 = pow (ix (IxX i False)) xdeg
            xi1 = pow (ix (IxX i True))  xdeg
            zix = pow1 (ix (IxZ i))
        return $ indexUnions [xi0, xi1, zix]
    cix = indexUnion yix rest

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
