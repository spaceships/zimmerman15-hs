{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Zim14.Circuit where

import Control.Monad.State
import qualified Data.Map as M

type Ref = Int
type ID  = Int
type Val = Int

data Op = Add Ref Ref
        | Sub Ref Ref
        | Mul Ref Ref
        | Const ID
        | Input ID
        deriving (Eq, Ord, Show)

data Circuit = Circuit { outRef    :: Ref
                       , inpRefs   :: M.Map ID Ref -- TODO: maybe dont need these maps
                       , refMap    :: M.Map Ref Op
                       , consts    :: [Integer]
                       } deriving (Show)

type TestCase = ([Int], Int)

depth :: Circuit -> Int
depth c = foldCirc f c
  where
    f (Input _) [] = 0
    f (Const _) [] = 0
    f _         xs = maximum xs + 1

degree :: Circuit -> Op -> Int
degree c z = foldCirc f c
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
evalMod c xs ys q = foldCirc evalOp c
  where
    evalOp (Add _ _) [x,y] = x + y `mod` q
    evalOp (Sub _ _) [x,y] = x - y `mod` q
    evalOp (Mul _ _) [x,y] = x * y `mod` q
    evalOp (Input id)   [] = xs !! id
    evalOp (Const id)   [] = ys !! id

-- note: inputs are little endian: [x0, x1, ..., xn]
evalTest :: Circuit -> [Int] -> Int
evalTest c xs = if foldCirc evalOp c /= 0 then 1 else 0 -- this is how the tests work
  where
    evalOp (Add _ _) [x,y] = x + y
    evalOp (Sub _ _) [x,y] = x - y
    evalOp (Mul _ _) [x,y] = x * y
    evalOp (Input id)   [] = xs !! id
    evalOp (Const id)   [] = fromIntegral (consts c !! id)

ensure :: Circuit -> [TestCase] -> Bool
ensure c ts = all ensure (zip [0..] ts)
  where
    ensure (i, (inps, res)) = if evalTest c (reverse inps) == res then True
                              else error ("test " ++ show i ++ " failed: " ++
                                          concatMap show inps ++ " /= " ++ show res)

foldCirc :: (Op -> [a] -> a) -> Circuit -> a
foldCirc f (Circuit {..}) = evalState (eval outRef) M.empty
  where
    eval ref = gets (M.lookup ref) >>= \case
        Just val -> return val
        Nothing  -> do
            let op = evilLook ref refMap
            argVals <- mapM eval (args op)
            let val = f op argVals
            modify (M.insert ref val)
            return val

evilLook :: (Ord a, Show a) => a -> M.Map a b -> b
evilLook x m = case M.lookup x m of
    Nothing -> error ("unknown ref " ++ show x ++ "!")
    Just op -> op

ninputs :: Circuit -> Int
ninputs = M.size . inpRefs

nconsts :: Circuit -> Int
nconsts = length . consts

args :: Op -> [Ref]
args (Add   x y) = [x,y]
args (Sub   x y) = [x,y]
args (Mul   x y) = [x,y]
args (Const _)   = []
args (Input _)   = []

