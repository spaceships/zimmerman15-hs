module Main where

import Zim14.IndexSet

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.Set as S

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests = [ testProperty "no overlapping symbols in indices" prop_indicesCorrect ]

prop_indicesCorrect :: Property
prop_indicesCorrect = forAll (choose (1, 128)) prop
  where
    prop n = let (nsyms, ixs) = allIndices n
             in S.fromList [0..nsyms-1] == S.fromList ixs

allIndices :: Int -> (Int, [Int])
allIndices n = (nsyms, y : xs ++ zs ++ ws ++ concat ss)
  where
    (ix, nsyms) = makeIndexer n
    y  = yindex ix
    xs = [ xindex ix i b   | b <- [0,1], i <- [0..n-1] ]
    zs = [ zindex ix i     | i <- [0..n-1] ]
    ws = [ windex ix i     | i <- [0..n-1] ]
    ss = [ sindex ix i b j | b <- [0,1], i <- [0..n-1], j <- [0..n-1] ]

dups :: [Int] -> [Int]
dups [] = []
dups (x:xs) = if x `elem` xs then x : dups xs else dups xs
