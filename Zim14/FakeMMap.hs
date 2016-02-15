{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Zim14.FakeMMap where

import Zim14.Circuit
import Zim14.Index
import Zim14.Obfuscate (Obfuscation)
import Zim14.Sym
import Zim14.Util (i2b)

import CLT13.Rand
import CLT13.Util (pmap)

import Control.DeepSeq (NFData)
import Data.Map ((!))
import Data.Monoid
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M

data FakeEncoding = FakeEncoding {
    ev  :: Integer,
    chk :: Integer,
    ix  :: Index
} deriving (Eq, Generic, NFData)

fakeEncode :: Integer -> Integer -> Index -> Rand FakeEncoding
fakeEncode x y ix = return $ FakeEncoding x y ix

fakeEval :: Obfuscation FakeEncoding -> Circuit -> [Int] -> Int
fakeEval obf c xs = undefined $ foldCirc eval (outRef c) c
  where
    eval :: Op -> [FakeEncoding] -> FakeEncoding
    eval (Input i)    [] = let b = i2b (xs !! i) in obf ! X_ i b
    eval (Const i)    [] = obf ! Y_ i
    eval (Mul _ _) [x,y] = fakeMul x y
    eval (Add _ _) [x,y] = fakeAdd obf x y
    eval (Sub _ _) [x,y] = fakeSub obf x y

fakeMul :: FakeEncoding -> FakeEncoding -> FakeEncoding
fakeMul x y = FakeEncoding (ev x * ev y) (chk x * chk y) (ix x <> ix y)

fakeProd :: [FakeEncoding] -> FakeEncoding
fakeProd = foldr1 fakeMul

fakeAdd :: Obfuscation FakeEncoding -> FakeEncoding -> FakeEncoding -> FakeEncoding
fakeAdd obf x y = FakeEncoding (ev x' + ev y') (chk x' + chk y') target
  where
    target = ix x <> ix y
    x' = raise obf target x
    y' = raise obf target y

fakeSub :: Obfuscation FakeEncoding -> FakeEncoding -> FakeEncoding -> FakeEncoding
fakeSub obf x y = FakeEncoding (ev x' - ev y') (chk x' - chk y') target
  where
    target = ix x <> ix y
    x' = raise obf target x
    y' = raise obf target y

-- raise x to the index target by multiplying by powers of U_ and V_
raise :: Obfuscation FakeEncoding -> Index -> FakeEncoding -> FakeEncoding
raise obf target x = accumIndex accum diff x
  where
    diff = indexDiff (ix x) target
    accum (X i b) x = fakeMul (obf!U_ i b) x
    accum Y       x = fakeMul (obf!V_)     x
    accum oops    _ = error ("[raise] unexpected index " ++ show oops)
