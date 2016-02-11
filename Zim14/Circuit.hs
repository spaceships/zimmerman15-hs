{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Zim14.Circuit where

import Control.Monad.State
import qualified Data.Map as M

type Ref = Int
type ID  = Int

data Op = Add Ref Ref
        | Sub Ref Ref
        | Mul Ref Ref
        | Const ID Int
        | Input ID
        deriving (Eq, Ord, Show)

data Circuit = Circuit { outRef    :: Ref
                       , inpRefs   :: M.Map ID Ref
                       , constRefs :: M.Map ID Ref
                       , refMap    :: M.Map Ref Op
                       } deriving (Show)

type TestCase = ([Int], Int)

eval :: Circuit -> [Int] -> Int
eval c xs = if foldCirc evalOp xs c /= 0 then 1 else 0 -- this is how the tests work
  where
    evalOp (Add _ _) [x,y] = x + y
    evalOp (Sub _ _) [x,y] = x - y
    evalOp (Mul _ _) [x,y] = x * y
    evalOp (Const _ x)  [] = x

depth :: Circuit -> Int
depth c = foldCirc f (replicate (ninputs c) 0) c
  where
    f (Const _ _) [] = 0
    f _           xs = maximum xs + 1

degree :: Circuit -> Op -> Int
degree c z = foldCirc f [] c
  where
    eq (Input x) (Input y) = x == y
    eq (Const _ _) (Const _ _) = True
    eq _ _ = False

    f (Add _ _) [x,y] = x + y
    f (Sub _ _) [x,y] = x + y
    f (Mul _ _) [x,y] = x + y
    f x [] = if eq x z then 1 else 0

ensure :: Circuit -> [TestCase] -> Bool
ensure c ts = all ensure (zip [0..] ts)
  where
    ensure (i, (inps, res)) = if eval c (reverse inps) == res then True
                              else error ("test " ++ show i ++ " failed: " ++
                                          concatMap show inps ++ " /= " ++ show res)

-- inps are little endian
foldCirc :: (Op -> [a] -> a) -> [a] -> Circuit -> a
foldCirc f inps (Circuit {..}) = evalState (eval outRef) known
  where
    known = M.fromList $ map (\(id, val) -> (look id inpRefs, val)) (zip [0..] inps)
    eval ref = do
        known <- get
        case M.lookup ref known of
            Just val -> return val
            Nothing -> do
                let op = look ref refMap
                argVals <- mapM eval (args op)
                let val = f op argVals
                put $ M.insert ref val known
                return val
    look x m = case M.lookup x m of
        Nothing -> error ("unknown ref " ++ show x ++ "!")
        Just op -> op

ninputs :: Circuit -> Int
ninputs = M.size . inpRefs

args :: Op -> [Ref]
args (Add   x y) = [x,y]
args (Sub   x y) = [x,y]
args (Mul   x y) = [x,y]
args (Const _ _) = []
args (Input _  ) = []

