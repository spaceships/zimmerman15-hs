{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Zim14.Circuit where

import Zim14.Util

import CLT13.Util (pmap)

import Control.Monad.State
import Data.Map.Strict ((!))
import Text.Printf
import qualified Data.Map.Strict as M

type Ref = Int
type ID  = Int
type Val = Int

data Op = Add Ref Ref
        | Sub Ref Ref
        | Mul Ref Ref
        | Const ID
        | Input ID
        deriving (Eq, Ord, Show)

data Circuit = Circuit { outRef  :: Ref
                       , inpRefs :: M.Map ID Ref -- TODO: maybe dont need these maps
                       , refMap  :: M.Map Ref Op
                       , consts  :: [Integer]
                       } deriving (Show)

type TestCase = ([Bool], Bool)

depth :: Circuit -> Int
depth c = foldCirc f (outRef c) c
  where
    f (Input _) [] = 0
    f (Const _) [] = 0
    f _         xs = maximum xs + 1

{-multDepth :: Circuit -> Int-}
{-multDepth c = foldCirc f (outRef c) c-}

degree :: Circuit -> Ref -> Op -> Int
degree c ref z = foldCirc f ref c
  where
    f (Add _ _) [x,y] = max x y
    f (Sub _ _) [x,y] = max x y
    f (Mul _ _) [x,y] = x + y
    f x _ = if eq x z then 1 else 0

    eq (Input x) (Input y) = x == y
    eq (Const _) (Const _) = True
    eq _         _         = False

-- note: inputs are little endian: [x0, x1, ..., xn]
evalMod :: Integral a => Circuit -> [a] -> [a] -> a -> a
evalMod c xs ys q = foldCirc eval (outRef c) c
  where
    eval (Add _ _) [x,y] = x + y % q
    eval (Sub _ _) [x,y] = x - y % q
    eval (Mul _ _) [x,y] = x * y % q
    eval (Input i)   [] = xs !! i
    eval (Const i)   [] = ys !! i
    eval _            _  = error "[evalMod] weird input"

-- note: inputs are little endian: [x0, x1, ..., xn]
plainEval :: Circuit -> [Bool] -> Bool
plainEval c xs = foldCirc eval (outRef c) c /= 0
  where
    eval :: Op -> [Integer] -> Integer
    eval (Add _ _) [x,y] = x + y
    eval (Sub _ _) [x,y] = x - y
    eval (Mul _ _) [x,y] = x * y
    eval (Input i)    [] = b2i (xs !! i)
    eval (Const i)    [] = fromIntegral (consts c !! i)
    eval _            _  = error "[plainEval] weird input"

ensure :: Bool -> (Circuit -> [Bool] -> Bool) -> Circuit -> [TestCase] -> IO Bool
ensure verbose eval c ts = and <$> mapM ensure' (zip [(0::Int)..] ts)
  where
    toBit :: Bool -> Char
    toBit b = if b then '1' else '0'

    ensure' (i, (inps, out)) = do
        let res = eval c (reverse inps)
        if res == out then do
            let s = printf "test %d succeeded: input:%s expected:%c got:%c"
                            i (map toBit inps) (toBit out) (toBit res)
            when verbose (putStrLn s)
            return True
        else do
            let s = printf "test %d failed! input:%s expected:%c got:%c"
                            i (map toBit inps) (toBit out) (toBit res)
            putStrLn (red s)
            return False

foldCirc :: (Op -> [a] -> a) -> Ref -> Circuit -> a
foldCirc f start (Circuit {..}) = evalState (eval start) M.empty
  where
    eval ref = gets (M.lookup ref) >>= \case
        Just val -> return val
        Nothing  -> do
            let op = refMap ! ref
            argVals <- mapM eval (args op)
            let val = f op argVals
            modify (M.insert ref val)
            return val

ninputs :: Circuit -> Int
ninputs = M.size . inpRefs

nconsts :: Circuit -> Int
nconsts = length . consts

ydeg :: Circuit -> Int
ydeg c = degree c (outRef c) (Const (-1))

xdeg :: Circuit -> Int -> Int
xdeg c i = xdegs c !! i

xdegs :: Circuit -> [Int]
xdegs c = pmap (degree c (outRef c) . Input) [0 .. ninputs c - 1]

args :: Op -> [Ref]
args (Add   x y) = [x,y]
args (Sub   x y) = [x,y]
args (Mul   x y) = [x,y]
args (Const _)   = []
args (Input _)   = []

