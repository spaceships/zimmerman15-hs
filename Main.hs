module Main where

import Zim14.Circuit
import Zim14.Circuit.Parser
import Zim14.Index
import Zim14.Obfuscate

import CLT13.Util (forceM)
import System.Environment

main = do
    [secparam, fn] <- getArgs
    let lambda = read secparam :: Int
    (c, ts) <- parseCirc <$> readFile fn
    o <- obfuscate lambda c
    return ()
