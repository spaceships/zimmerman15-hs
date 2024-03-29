module Zim15.Evaluate where

import Zim15.Circuit
import Zim15.Encoding
import Zim15.Index
import Zim15.Obfuscate (Obfuscation)
import Zim15.Sym
import Zim15.Util (red)

import Control.DeepSeq (NFData)
import Data.Map ((!))
import Data.Monoid
import Debug.Trace
import Text.Printf

data ObfEvaluator a = ObfEvaluator {
    evMul :: a -> a -> a,
    evAdd :: a -> a -> a,
    evSub :: a -> a -> a
}

eval :: (Show a, NFData a) => ObfEvaluator a -> Obfuscation a -> Circuit -> [Bool] -> IO a
eval ev obf c xs | constNegation c = error "[eval] const negation"
                 | otherwise = do
    (chat, _) <- foldCircIO eval' c
    let z = mul ev (strictSub ev (mul ev chat zhat) (mul ev (obf!C_) what)) σ

    let tl   = topLevelIndex c
    let tl'  = ix z
    let diff = indexDiff tl tl'

    return $ if not (indexEq tl tl') then
        trace (red ("[eval] top level index not reached. missing indices: " ++ show diff))
        trace ("z = " ++ show z) (val z)
    else
        val z
  where
    n = ninputs c

    eval' (Input i) []               = (obf ! X_ i (xs!!i), obf ! U_ i (xs!!i))
    eval' (Const i) []               = (obf ! Y_ i, obf ! V_)
    eval' (Mul _ _) [(x,ox), (y,oy)] = (mul ev x y, mul ev ox oy)
    eval' (Add _ _) [(x,ox), (y,oy)] = (add ev obf x y, add ev obf ox oy)
    eval' (Sub 0 _) [(x,ox)]         = (strictSub ev ox x, ox)
    eval' (Sub _ _) [(x,ox), (y,oy)] = (sub ev obf x y, sub ev obf ox oy)
    eval' _         _                = error "[eval] weird input"

    prod = foldr1 (mul ev)

    σ = prod [ obf ! S_ i1 i2 (xs!!i1) (xs!!i2)
             | i1 <- [0..n-1], i2 <- [0..n-1], i1 < i2
             ]
    zhat = prod [ obf ! Z_ i (xs!!i) | i <- [0..n-1] ]
    what = prod [ obf ! W_ i (xs!!i) | i <- [0..n-1] ]

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

strictSub :: Show a => ObfEvaluator a -> Encoding a -> Encoding a -> Encoding a
strictSub ev x y
    | not (indexEq (ix x) (ix y)) = error ("[strictSub] arguments with different indices!\n" ++
                                           show x ++ "\n" ++ show y)
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
    accum (X i b) = mul ev (obf ! U_ i b)
    accum Y       = mul ev (obf ! V_)
    accum _       = id
