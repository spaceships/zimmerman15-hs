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
    pows = powMap c xs

    eval :: Op -> [FakeEncoding] -> FakeEncoding
    eval (Input i)    [] = let b = i2b (xs !! i) in obf ! X_ i b
    eval (Const i)    [] = obf ! Y_ i
    eval (Mul _ _) [x,y] = fakeMul x y
    eval (Add _ _) [x,y] = fakeAdd obf pows x y
    eval (Sub _ _) [x,y] = fakeSub obf x y

type PowMap = M.Map Ref [(Sym, Int)]
-- returns a Map of each of the Syms in an index at a particular point in the
-- circuit. Requires input to decide between X i True and X i False.
powMap :: Circuit -> [Int] -> PowMap
powMap c xs = M.fromList $ pmap pows $ M.keys (refMap c)
  where
    n = ninputs c
    m = nconsts c
    getXs ref = [(X_ i (i2b (xs!!i)), degree c ref (Input i)) | i <- [0..n]]
    getYs ref = [(Y_ j              , degree c ref (Const j)) | j <- [0..m]]
    pows  ref = (ref, getXs ref ++ getYs ref)

fakeMul :: FakeEncoding -> FakeEncoding -> FakeEncoding
fakeMul x y = FakeEncoding (ev x * ev y) (chk x * chk y) (ix x <> ix y)

fakeProd :: [FakeEncoding] -> FakeEncoding
fakeProd = foldr1 fakeMul

fakeAdd
  :: Obfuscation FakeEncoding
  -> PowMap
  -> FakeEncoding
  -> FakeEncoding
  -> FakeEncoding
fakeAdd obf pows x y = undefined
  where
    zix   = ix x <> ix y
    xdiff = indexDiff zix (ix x)
    ydiff = indexDiff zix (ix y)

fakeSub = undefined

-- TODO: I think I need to keep track of FORMAL SYMBOLS in order to lift encodings
