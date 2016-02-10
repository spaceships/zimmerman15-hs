{-# LANGUAGE RecordWildCards #-}

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

evalOp :: Op -> [Int] -> Int
evalOp (Add _ _) [x,y] = x + y
evalOp (Sub _ _) [x,y] = x - y
evalOp (Mul _ _) [x,y] = x * y
evalOp (Const _ x)  [] = x
evalOp (Input _)     _ = error "evalOp doesn't know how to evaluate input"

evalCirc :: Circ -> [Int] -> Int
evalCirc (Circ {..}) inps = evalState (eval outRef) known
  where
    known = M.fromList $ map (\(id, ref) -> (ref, inps !! id)) (M.toList inpRefs)

    eval :: Ref -> State (M.Map Ref Int) Int
    eval ref = do
        known <- get
        case M.lookup ref known of
            Just result -> return result
            Nothing -> do
                let op = look ref
                val <- evalOp op <$> mapM eval (args op)
                put $ M.insert ref val known
                return val

    look ref = case M.lookup ref refMap of
        Nothing -> error ("unknown ref " ++ show ref ++ "!")
        Just op -> op

ensureCirc :: Circ -> [TestCase] -> Bool
ensureCirc c ts = all ensure (zip [0..] ts)
  where
    ensure (i, (inps, res)) = if evalCirc c inps == res then True
                              else error ("test " ++ show i ++ " failed: " ++
                                          concatMap show inps ++ " /= " ++ show res)
