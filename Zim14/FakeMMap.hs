{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Zim14.FakeMMap where

import Zim14.Circuit
import Zim14.Index
import Zim14.Obfuscate (Obfuscation, Params(..))
import Zim14.Sym
import Zim14.Util (i2b, b2i)

import CLT13.Rand

import Control.DeepSeq (NFData)
import Data.Map ((!))
import Data.Monoid
import GHC.Generics (Generic)
import Text.Printf
import qualified Data.Map.Strict as M

import Debug.Trace

data FakeEncoding = FakeEncoding {
    ev  :: Integer,
    chk :: Integer,
    ix  :: Index
} deriving (Eq, Generic, NFData)

instance Show FakeEncoding where
    show (FakeEncoding {..}) = printf "[%d, %d](%s)" ev chk (show ix)

fakeEncode :: Integer -> Integer -> Index -> Rand FakeEncoding
fakeEncode x y ix = return $ FakeEncoding x y ix

fakeEvalTest :: Obfuscation FakeEncoding -> Params -> Circuit -> [Int] -> Int
fakeEvalTest obf p c xs = b2i $ not ((ev z == 0) && (chk z == 0))
  where
    z = fakeEval obf p c (map i2b xs)

fakeEval :: Obfuscation FakeEncoding -> Params -> Circuit -> [Bool] -> FakeEncoding
fakeEval obf p@(Params {..}) c xs =
    if not (tl == tl') then
        trace ("top level index not reached: " ++ show (M.difference tl' tl))
        trace ("z = " ++ show z) z
    else
        z
  where
    tl  = topLevelCLTIndex c
    tl' = indexer n (ix z)

    chat = let res = foldCirc eval (outRef c) c
           in trace ("chat = " ++ show res) res

    σ = fakeProd p [ obf ! S_ i1 i2 (xs!!i1) (xs!!i2)
                   | i1 <- [0..n-1], i2 <- [0..n-1], i1 < i2
                   ]

    zhat = fakeProd p [ obf ! Z_ i (xs!!i) | i <- [0..n-1] ]
    what = fakeProd p [ obf ! W_ i (xs!!i) | i <- [0..n-1] ]

    z = fakeMul p (fakeSub obf p (fakeMul p chat zhat) (fakeMul p (obf!C_) what)) σ

    eval :: Op -> [FakeEncoding] -> FakeEncoding
    eval (Input i)    [] = let b = (xs !! i) in obf ! X_ i b
    eval (Const i)    [] = obf ! Y_ i
    eval (Mul _ _) [x,y] = fakeMul p x y
    eval (Add _ _) [x,y] = fakeAdd obf p x y
    eval (Sub _ _) [x,y] = fakeSub obf p x y
    eval _         _     = error "[fakeEval] weird input"

fakeMul :: Params -> FakeEncoding -> FakeEncoding -> FakeEncoding
fakeMul p x y = FakeEncoding ev' chk' ix'
  where
    ev'  = ev x  * ev y  `mod` n_ev p
    chk' = chk x * chk y `mod` n_chk p
    ix'  = ix x <> ix y

fakeProd :: Params -> [FakeEncoding] -> FakeEncoding
fakeProd p = foldr1 (fakeMul p)

fakeAdd :: Obfuscation FakeEncoding -> Params -> FakeEncoding -> FakeEncoding -> FakeEncoding
fakeAdd obf p x y = FakeEncoding ev' chk' target
  where
    target = ix x <> ix y
    x' = raise obf p target x
    y' = raise obf p target y
    ev'  = ev x'  + ev y'  `mod` n_ev p
    chk' = chk x' + chk y' `mod` n_chk p


fakeSub :: Obfuscation FakeEncoding -> Params -> FakeEncoding -> FakeEncoding -> FakeEncoding
fakeSub obf p x y = FakeEncoding ev' chk' target
  where
    target = ix x <> ix y
    x' = raise obf p target x
    y' = raise obf p target y
    ev'  = ev x'  - ev y'  `mod` n_ev p
    chk' = chk x' - chk y' `mod` n_chk p

-- raise x to the index target by multiplying by powers of U_ and V_
raise :: Obfuscation FakeEncoding -> Params -> Index -> FakeEncoding -> FakeEncoding
raise obf p target x = accumIndex accum diff x
  where
    diff = indexDiff (ix x) target
    accum (X i b) = fakeMul p (obf!U_ i b)
    accum Y       = fakeMul p (obf!V_)
    accum oops    = error ("[raise] unexpected index " ++ show oops)
