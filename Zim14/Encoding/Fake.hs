{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Zim14.Encoding.Fake where

import Zim14.Circuit
import Zim14.Evaluate
import Zim14.Obfuscate (Obfuscation, Encoder, Params (n_ev, n_chk))
import Zim14.Util ((%))

import Control.DeepSeq (NFData)
import Data.Serialize
import GHC.Generics (Generic)
import Text.Printf

data Fake = Fake {
    ev  :: Integer,
    chk :: Integer
} deriving (Eq, Generic, Serialize, NFData)

instance Show Fake where
    show (Fake {..}) = printf "[%d, %d]" ev chk

-- fakeEncode :: Integer -> Integer -> Index -> Rand Fake
fakeEncode :: Encoder Fake
fakeEncode x y _ = return $ Fake x y

fakeEval :: Obfuscation Fake -> Params -> Circuit -> [Bool] -> IO Bool
fakeEval obf p c xs = do
    res <- eval fakeEv obf c xs
    return $ (ev res /= 0) || (chk res /= 0)
  where
    fakeEv = ObfEvaluator {
        evMul = fakeMul p,
        evAdd = fakeAdd p,
        evSub = fakeSub p
    }

fakeMul :: Params -> Fake -> Fake -> Fake
fakeMul p x y = Fake ev' chk'
  where
    ev'  = ev x  * ev y  % n_ev p
    chk' = chk x * chk y % n_chk p

fakeAdd :: Params -> Fake -> Fake -> Fake
fakeAdd p x y = Fake ev' chk'
  where
    ev'  = ev x  + ev y  % n_ev p
    chk' = chk x + chk y % n_chk p

fakeSub :: Params -> Fake -> Fake -> Fake
fakeSub p x y = Fake ev' chk'
  where
    ev'  = ev x  - ev y  % n_ev p
    chk' = chk x - chk y % n_chk p
