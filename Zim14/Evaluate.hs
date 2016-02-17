module Zim14.Evaluate where

import Zim14.Circuit
import Zim14.Encoding
import Zim14.Index
import Zim14.Obfuscate (Obfuscation)
import Zim14.Sym
import Zim14.Util (red, b2i)

import Data.Map ((!))
import Data.Monoid
import Debug.Trace
import Text.Printf

data ObfEvaluator a = ObfEvaluator {
    evMul :: a -> a -> a,
    evAdd :: a -> a -> a,
    evSub :: a -> a -> a
}

eval :: Show a => ObfEvaluator a -> Obfuscation a -> Circuit -> [Bool] -> a
eval ev obf c xs =
    if not (indexEq tl tl') then
        trace (red ("[eval] top level index not reached. missing indices: " ++ show diff))
        trace ("z = " ++ show z) (val z)
    else
        trace ("z = " ++ show z) (val z)
  where
    n = ninputs c

    eval' (Input i)    [] = let b   = (xs !! i) in obf ! X_ i b
    eval' (Const i)    [] = obf ! Y_ i
    eval' (Mul _ _) [x,y] = mul ev x y
    eval' (Add _ _) [x,y] = add ev obf x y
    eval' (Sub _ _) [x,y] = sub ev obf x y
    eval' _         _     = error "[eval] weird input"

    prod = foldr1 (mul ev)

    chat = foldCirc eval' (outRef c) c
    σ = prod [ obf ! S_ i1 i2 (xs!!i1) (xs!!i2)
             | i1 <- [0..n-1], i2 <- [0..n-1], i1 < i2
             ]
    zhat = prod [ obf ! Z_ i (xs!!i) | i <- [0..n-1] ]
    what = prod [ obf ! W_ i (xs!!i) | i <- [0..n-1] ]
    z = mul ev (strictSub ev obf (mul ev chat zhat) (mul ev (obf!C_) what)) σ

    tl   = topLevelIndex c
    tl'  = ix z
    diff = indexDiff tl tl'

mul :: ObfEvaluator a -> Encoding a -> Encoding a -> Encoding a
mul ev x y = Encoding ix' val'
  where
    val' = evMul ev (val x) (val y)
    ix'  = ix x <> ix y

add :: ObfEvaluator a -> Obfuscation a -> Encoding a -> Encoding a -> Encoding a
add ev obf x y = Encoding target val'
  where
    target = indexUnion (ix x) (ix y)
    x' = raise ev obf target x
    y' = raise ev obf target y
    val' = evAdd ev (val x') (val y')

strictSub :: ObfEvaluator a -> Obfuscation a -> Encoding a -> Encoding a -> Encoding a
strictSub ev obf x y
    | not (indexEq (ix x) (ix y)) = error "[strictSub] arguments with different indices!"
    | otherwise = Encoding (ix x) val'
  where
    val' = evSub ev (val x) (val y)

sub :: ObfEvaluator a -> Obfuscation a -> Encoding a -> Encoding a -> Encoding a
sub ev obf x y = Encoding target val'
  where
    target = indexUnion (ix x) (ix y)
    x' = raise ev obf target x
    y' = raise ev obf target y
    val' = evSub ev (val x') (val y')

-- raise x to the index target by multiplying by powers of U_ and V_
raise :: ObfEvaluator a -> Obfuscation a -> Index -> Encoding a -> Encoding a
raise ev obf target x = accumIndex accum diff x
  where
    diff = indexMinus target (ix x)
    accum (X i b) = mul ev (obf!U_ i b)
    accum Y       = mul ev (obf!V_)
    accum _       = id
