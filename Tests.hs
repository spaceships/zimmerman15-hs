module Main where

import Zim14.Index

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.Set as S

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests = [ testProperty "no overlapping symbols in indices" prop_indicesCorrect
        , testProperty "sindices correct" prop_sindicesCorrect
        ]

prop_indicesCorrect :: Property
prop_indicesCorrect = forAll (choose (1, 128)) $ \n ->
    let (nsyms, ixs) = allIndices n
    in S.fromList [0..nsyms-1] == S.fromList ixs

allIndices :: Int -> (Int, [Index])
allIndices n = (nsyms, y : xs ++ zs ++ ws ++ concat ss)
  where
    (ix, nsyms) = indexer n
    y  = yindex ix
    xs = [ xindex ix i b   | b <- [False, True], i <- [0..n-1] ]
    zs = [ zindex ix i     | i <- [0..n-1] ]
    ws = [ windex ix i     | i <- [0..n-1] ]
    ss = [ sindex ix i b j | b <- [False, True], i <- [0..n-1], j <- [0..n-1] ]

prop_sindicesCorrect :: Property
prop_sindicesCorrect = forAll (choose (1, 128)) $ \n ->
    let (ix, nzs) = indexer n
        ss  = concat [sindex ix i b j | b <- [False, True], i <- [0..n-1], j <- [0..n-1]]
        ss' = concat [sindices ix i   | i <- [0..n-1]]
    in S.fromList ss == S.fromList ss'