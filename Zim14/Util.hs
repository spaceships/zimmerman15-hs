module Zim14.Util where

import CLT13.Util (sizeBase2)

import Data.Bits ((.&.))
import System.IO

b2i :: Integral a => Bool -> a
b2i False = 0
b2i True  = 1

i2b :: Integral a => a -> Bool
i2b = not . (== 0)

pr :: String -> IO ()
pr s = do
    putStrLn s
    hFlush stdout

num2bits :: Integer -> [Bool]
num2bits x = reverse bs
  where
    bs = [ x .&. 2^i > 0 | i <- [0 .. sizeBase2 x] ]

readBitstring :: String -> [Bool]
readBitstring = map (== '1')

red :: String -> String
red s = "\x1b[1;41m" ++ s ++ "\x1b[0m"
