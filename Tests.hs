module Main where

import Zim14.Index
import Zim14.Circuit
import Zim14.Circuit.Parser
import Zim14.Encoding.CLT13
import Zim14.Util

import CLT13.Rand
import qualified CLT13 as CLT

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests = [ testProperty "top level encoding zero tests correctly" prop_zeroTestTopLevel
        ]

prop_zeroTestTopLevel :: Property
prop_zeroTestTopLevel = monadicIO $ do
    x <- pick arbitrary
    y <- pick arbitrary
    λ <- pick $ choose (4, 16)
    z <- run $ zeroTestTopLevel "circuits/add.acirc" λ (b2i x) (b2i y)
    let isZero False False = True
        isZero _ _         = False
    assert (isZero x y == z)

zeroTestTopLevel :: FilePath -> Int -> Integer -> Integer -> IO Bool
zeroTestTopLevel fp λ x y = do
    (c, _) <- parseCirc <$> readFile fp
    let n = ninputs c
        d = depth c
    mmap <- CLT.setup True (λ+d) d (numIndices n) (topLevelCLTIndex c)
    let pp  = CLT.publicParams mmap
        enc = cltEncode mmap (indexer c)
    x <- randIO (enc x y (topLevelIndex c))
    return $ CLT.isZero pp (getCLT x)
