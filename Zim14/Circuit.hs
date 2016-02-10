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

data Circ = Circ { outRef    :: Ref
                 , inpRefs   :: M.Map ID Ref
                 , constRefs :: M.Map ID Ref
                 , refMap    :: M.Map Ref Op
                 } deriving (Show)

type TestCase = ([Int], Int)

args :: Op -> [Ref]
args (Add   x y) = [x,y]
args (Sub   x y) = [x,y]
args (Mul   x y) = [x,y]
args (Const _ _) = []
args (Input _  ) = []

foldCirc :: (Op -> [a] -> a) -> [a] -> Circ -> a
foldCirc f inps (Circ {..}) = evalState (eval outRef) known
  where
    known = M.fromList $ map (\(id, ref) -> (ref, inps !! id)) (M.toList inpRefs)

    eval ref = do
        known <- get
        case M.lookup ref known of
            Just val -> return val
            Nothing -> do
                let op = look ref
                argVals <- mapM eval (args op)
                let val = f op argVals
                put $ M.insert ref val known
                return val

    look ref = case M.lookup ref refMap of
        Nothing -> error ("unknown ref " ++ show ref ++ "!")
        Just op -> op

evalCirc :: Circ -> [Int] -> Int
evalCirc c xs = if foldCirc evalOp xs c /= 0 then 1 else 0 -- this is how the tests work
  where
    evalOp (Add _ _) [x,y] = x + y
    evalOp (Sub _ _) [x,y] = x - y
    evalOp (Mul _ _) [x,y] = x * y
    evalOp (Const _ x)  [] = x

circDepth :: Circ -> Int
circDepth = foldCirc f [0..]
  where
    f (Input _  ) [] = 0
    f (Const _ _) [] = 0
    f _           xs = maximum xs + 1

ensureCirc :: Circ -> [TestCase] -> Bool
ensureCirc c ts = all ensure (zip [0..] ts)
  where
    ensure (i, (inps, res)) = if evalCirc c inps == res then True
                              else error ("test " ++ show i ++ " failed: " ++
                                          concatMap show inps ++ " /= " ++ show res)
