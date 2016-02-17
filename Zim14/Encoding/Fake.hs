{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Zim14.Encoding.Fake where

import Zim14.Index
import Zim14.Encoding
import Zim14.Obfuscate (Obfuscation, Params)
import Zim14.Evaluate
import Zim14.Circuit

import CLT13.Rand

import Control.DeepSeq (NFData)
import Data.Monoid
import Data.Serialize
import GHC.Generics (Generic)
import Text.Printf

data Fake = Fake {
    ev  :: Integer,
    chk :: Integer
} deriving (Eq, Generic, Serialize, NFData)

instance Show Fake where
    show (Fake {..}) = printf "[%d, %d]" ev chk

fakeEncode :: Integer -> Integer -> Index -> Rand Fake
fakeEncode x y ix = return $ Fake x y

{-fakeEvalTest :: Obfuscation Fake -> Params -> Circuit -> [Int] -> Int-}
{-fakeEvalTest obf p c xs = b2i $ not ((ev z == 0) && (chk z == 0))-}
  {-where-}
    {-z = eval fakeEv obf p c (map i2b xs)-}

fakeEval :: Obfuscation Fake -> Params -> Circuit -> [Bool] -> Bool
fakeEval = undefined

fakeEv :: ObfEvaluator Fake
fakeEv = ObfEvaluator {
    evAdd = undefined,
    evSub = undefined,
    evMul = undefined,
    evExtract = undefined
}

{-fakeEval :: Obfuscation FakeEncoding -> Params -> Circuit -> [Bool] -> FakeEncoding-}
{-fakeEval obf p@(Params {..}) c xs =-}
    {-if not (indexEq tl tl') then-}
        {-trace (red ("top level index not reached. missing indices: " ++ show diff))-}
        {-trace ("z = " ++ show z) z-}
    {-else-}
        {-z-}
  {-where-}
    {-eval :: Op -> [FakeEncoding] -> FakeEncoding-}
    {-eval (Input i)    [] = let b = (xs !! i) in obf ! X_ i b-}
    {-eval (Const i)    [] = obf ! Y_ i-}
    {-eval (Mul _ _) [x,y] = fakeMul p x y-}
    {-eval (Add _ _) [x,y] = fakeAdd obf p x y-}
    {-eval (Sub _ _) [x,y] = fakeSub obf p x y-}
    {-eval _         _     = error "[fakeEval] weird input"-}

    {-chat = foldCirc eval (outRef c) c-}
    {-σ = fakeProd p [ obf ! S_ i1 i2 (xs!!i1) (xs!!i2)-}
                   {-| i1 <- [0..n-1], i2 <- [0..n-1], i1 < i2-}
                   {-]-}
    {-zhat = fakeProd p [ obf ! Z_ i (xs!!i) | i <- [0..n-1] ]-}
    {-what = fakeProd p [ obf ! W_ i (xs!!i) | i <- [0..n-1] ]-}
    {-z = fakeMul p (fakeSub obf p (fakeMul p chat zhat) (fakeMul p (obf!C_) what)) σ-}

    {-tl   = topLevelIndex c-}
    {-tl'  = ix z-}
    {-diff = indexDiff tl tl'-}

{-fakeMul :: Params -> FakeEncoding -> FakeEncoding -> FakeEncoding-}
{-fakeMul p x y = FakeEncoding ev' chk' ix'-}
  {-where-}
    {-ev'  = ev x  * ev y  `mod` n_ev p-}
    {-chk' = chk x * chk y `mod` n_chk p-}
    {-ix'  = ix x <> ix y-}

{-fakeProd :: Params -> [FakeEncoding] -> FakeEncoding-}
{-fakeProd p = foldr1 (fakeMul p)-}

{-fakeAdd :: Obfuscation FakeEncoding -> Params -> FakeEncoding -> FakeEncoding -> FakeEncoding-}
{-fakeAdd obf p x y = FakeEncoding ev' chk' target-}
  {-where-}
    {-target = ix x <> indexMinus (ix y) (ix x)-}
    {-x' = fakeRaise obf p target x-}
    {-y' = fakeRaise obf p target y-}
    {-ev'  = ev x'  + ev y'  `mod` n_ev p-}
    {-chk' = chk x' + chk y' `mod` n_chk p-}

{-fakeSub :: Obfuscation FakeEncoding -> Params -> FakeEncoding -> FakeEncoding -> FakeEncoding-}
{-fakeSub obf p x y = FakeEncoding ev' chk' target-}
  {-where-}
    {-target = ix x <> indexMinus (ix y) (ix x)-}
    {-x' = fakeRaise obf p target x-}
    {-y' = fakeRaise obf p target y-}
    {-ev'  = ev x'  - ev y'  `mod` n_ev p-}
    {-chk' = chk x' - chk y' `mod` n_chk p-}

{--- raise x to the index target by multiplying by powers of U_ and V_-}
{-fakeRaise :: Obfuscation FakeEncoding -> Params -> Index -> FakeEncoding -> FakeEncoding-}
{-fakeRaise obf p target x = accumIndex accum diff x-}
  {-where-}
    {-diff = indexDiff (ix x) target-}

    {-accum (X i b) = fakeMul p (obf!U_ i b)-}
    {-accum Y       = fakeMul p (obf!V_)-}
    {-accum _       = id-}
