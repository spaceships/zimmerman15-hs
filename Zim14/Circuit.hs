{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Zim14.Circuit where

import Zim14.Util

import CLT13.Util (pmap)

import Control.Monad.State
import Data.Map.Strict ((!))
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

type TestCase = ([Int], Int)

type Evaluator = Circuit -> [Int] -> Int

depth :: Circuit -> Int
depth c = foldCirc f (outRef c) c
  where
    f (Input _) [] = 0
    f (Const _) [] = 0
    f _         xs = maximum xs + 1

degree :: Circuit -> Ref -> Op -> Int
degree c ref z = foldCirc f ref c
  where
    f (Add _ _) [x,y] = x + y
    f (Sub _ _) [x,y] = x + y
    f (Mul _ _) [x,y] = x + y
    f x [] = if eq x z then 1 else 0

    eq (Input x) (Input y) = x == y
    eq (Const _) (Const _) = True
    eq _         _         = False

-- note: inputs are little endian: [x0, x1, ..., xn]
evalMod :: Integral a => Circuit -> [a] -> [a] -> a -> a
evalMod c xs ys q = foldCirc eval (outRef c) c
  where
    eval (Add _ _) [x,y] = x + y `mod` q
    eval (Sub _ _) [x,y] = x - y `mod` q
    eval (Mul _ _) [x,y] = x * y `mod` q
    eval (Input id)   [] = xs !! id
    eval (Const id)   [] = ys !! id

-- note: inputs are little endian: [x0, x1, ..., xn]
plainEval :: Circuit -> [Int] -> Int
plainEval c xs = b2i (foldCirc eval (outRef c) c /= 0) -- this is how the tests work
  where
    eval (Add _ _) [x,y] = x + y
    eval (Sub _ _) [x,y] = x - y
    eval (Mul _ _) [x,y] = x * y
    eval (Input id)   [] = xs !! id
    eval (Const id)   [] = fromIntegral (consts c !! id)

ensure :: Evaluator -> Circuit -> [TestCase] -> IO Bool
ensure eval c ts = and <$> mapM ensure (zip [0..] ts)
  where
    ensure (i, (inps, res)) = do
        if eval c (reverse inps) == res then
            return True
        else do
            putStrLn ("\x1b[1;41m" ++ "test " ++ show i ++ " failed: " ++
                      concatMap show inps ++ " /= " ++ show res ++ "\x1b[0m")
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

xdegs :: Circuit -> [Int]
xdegs c = pmap (degree c (outRef c) . Input) [0 .. ninputs c - 1]

args :: Op -> [Ref]
args (Add   x y) = [x,y]
args (Sub   x y) = [x,y]
args (Mul   x y) = [x,y]
args (Const _)   = []
args (Input _)   = []

