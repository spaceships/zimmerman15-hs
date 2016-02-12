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

tests = [ testProperty "all the indices are used" prop_allIndicesUsed
        , testProperty "xindices correctly returns all xindices" prop_xindicesCorrect
        , testProperty "zindices correctly returns all zindices" prop_zindicesCorrect
        , testProperty "windices correctly returns all windices" prop_windicesCorrect
        , testProperty "sindices correctly returns all sindices" prop_sindicesCorrect
        ]

prop_allIndicesUsed :: Property
prop_allIndicesUsed = forAll (choose (1, 128)) $ \n ->
    let (nsyms, ixs) = allIndices n
    in S.fromList [0..nsyms-1] == S.fromList ixs

allIndices :: Int -> (Int, [Index])
allIndices n = (nsyms, y ++ xs ++ zs ++ ws ++ ss)
  where
    ix    = indexer n
    nsyms = nindices n
    y  = ix SymY
    xs = concat [ix (SymX i b)   | b <- [False, True], i <- [0..n-1]]
    zs = concat [ix (SymZ i)     | i <- [0..n-1]]
    ws = concat [ix (SymW i)     | i <- [0..n-1]]
    ss = concat [ix (SymS i b j) | b <- [False, True], i <- [0..n-1], j <- [0..n-1]]

prop_xindicesCorrect :: Property
prop_xindicesCorrect = forAll (choose (1, 128)) $ \n ->
    let ix  = indexer n
        xs  = concat [ix (SymX i b) | b <- [False, True], i <- [0..n-1]]
        xs' = xindices n
    in S.fromList xs == S.fromList xs'

prop_zindicesCorrect :: Property
prop_zindicesCorrect = forAll (choose (1, 128)) $ \n ->
    let ix  = indexer n
        zs  = concat [ix (SymZ i) | i <- [0..n-1]]
        zs' = zindices n
    in S.fromList zs == S.fromList zs'

prop_windicesCorrect :: Property
prop_windicesCorrect = forAll (choose (1, 128)) $ \n ->
    let ix  = indexer n
        ws  = concat [ix (SymW i) | i <- [0..n-1]]
        ws' = windices n
    in S.fromList ws == S.fromList ws'

prop_sindicesCorrect :: Property
prop_sindicesCorrect = forAll (choose (1, 128)) $ \n ->
    let ix  = indexer n
        ss  = concat [ix (SymS i b j) | b <- [False, True], i <- [0..n-1], j <- [0..n-1]]
        ss' = sindices n
    in S.fromList ss == S.fromList ss'
