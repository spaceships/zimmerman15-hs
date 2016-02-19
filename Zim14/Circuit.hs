{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Zim14.Circuit where

import Zim14.Util

import CLT13.Util (forceM, pmap)

import Control.Monad.Identity
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.ParallelIO
import Control.DeepSeq (NFData)
import Control.Monad.IfElse (whenM)
import Control.Monad.State.Strict
import Data.Map.Strict ((!))
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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

-- note: inputs are little endian: [x0, x1, ..., xn]
plainEvalIO :: Circuit -> [Bool] -> IO Bool
plainEvalIO c xs = do
    z <- foldCircIO eval c
    return $ z /= 0
  where
    eval :: Op -> [Integer] -> Integer
    eval (Add _ _) [x,y] = x + y
    eval (Sub _ _) [x,y] = x - y
    eval (Mul _ _) [x,y] = x * y
    eval (Input i)    [] = b2i (xs !! i)
    eval (Const i)    [] = fromIntegral (consts c !! i)
    eval _            _  = error "[plainEval] weird input"

ensure :: Bool -> (Circuit -> [Bool] -> IO Bool) -> Circuit -> [TestCase] -> IO Bool
ensure verbose eval c ts = and <$> mapM ensure' (zip [(0::Int)..] ts)
  where
    toBit :: Bool -> Char
    toBit b = if b then '1' else '0'

    ensure' (i, (inps, out)) = do
        res <- eval c (reverse inps)
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
foldCirc f start c = runIdentity (foldCircM f' start c)
  where
    f' op _ xs = return (f op xs)

foldCircM :: Monad m => (Op -> Ref -> [a] -> m a) -> Ref -> Circuit -> m a
foldCircM f start (Circuit {..}) = evalStateT (eval start) M.empty
  where
    eval ref = gets (M.lookup ref) >>= \case
        Just val -> return val
        Nothing  -> do
            let op = refMap ! ref
            argVals <- mapM eval (opArgs op)
            val     <- lift (f op ref argVals)
            modify (M.insert ref val)
            return val

-- evaluate the circuit in parallel
foldCircIO :: NFData a => (Op -> [a] -> a) -> Circuit -> IO a
foldCircIO f c = do
    let refs = M.keys (refMap c)
    mem <- (M.fromList . zip refs) <$> replicateM (length refs) newEmptyTMVarIO
    let eval :: Ref -> IO ()
        eval ref = do
            let op      = refMap c ! ref
                argRefs = map (mem!) (opArgs op)
            -- this condition should never be hit since we parallelize over the topological levels
            whenM (or <$> mapM (atomically . isEmptyTMVar) argRefs) $ do
                putStrLn "blocking!"
                yield
                eval ref
            argVals <- mapM (atomically . readTMVar) argRefs
            let val = f op argVals
            forceM val
            atomically $ putTMVar (mem ! ref) val
    let lvls = topoLevels c
    forceM lvls
    forM_ (zip [(0 :: Int)..] lvls) $ \(_, lvl) -> do
        {-printf "evaluating level %d size=%d\n" i (length lvl)-}
        parallelInterleaved (map eval lvl)
    atomically (readTMVar (mem ! outRef c))

topologicalOrder :: Circuit -> [Ref]
topologicalOrder c = reverse $ execState (foldCircM eval (outRef c) c) []
  where
    eval :: Op -> Ref -> [a] -> State [Ref] ()
    eval _ ref _ = modify (ref:)

topoLevels :: Circuit -> [[Ref]]
topoLevels c = map S.toAscList lvls
  where
    topo = topologicalOrder c
    lvls = execState (mapM_ eval topo) []

    eval :: Ref -> State [S.Set Ref] ()
    eval ref = modify (putRef ref)

    putRef :: Ref -> [S.Set Ref] -> [S.Set Ref]
    putRef ref []     = [S.singleton ref]
    putRef ref (x:xs) = if not (any (`S.member` x) (dependencies ref))
                            then S.insert ref x : xs
                            else x : putRef ref xs

    dependencies :: Ref -> [Ref]
    dependencies ref = case refMap c ! ref of
        Input _ -> []
        Const _ -> []
        op      -> opArgs op ++ concatMap dependencies (opArgs op)

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

opArgs :: Op -> [Ref]
opArgs (Add   x y) = [x,y]
opArgs (Sub   x y) = [x,y]
opArgs (Mul   x y) = [x,y]
opArgs (Const _)   = []
opArgs (Input _)   = []

