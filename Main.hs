{-# LANGUAGE RecordWildCards #-}

module Main where

import Zim14.Circuit
import Zim14.Circuit.Parser
import Zim14.Index
import Zim14.Obfuscate
import Zim14.FakeMMap

import System.Environment
import qualified CLT13 as CLT

import Options

data MainOptions = MainOptions { fakeMMap :: Bool
                               , lambda   :: Int
                               , verbose  :: Bool
                               }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = ['f']
                     , optionLongFlags   = ["fake"]
                     , optionDescription = "Use a fake mmap for testing."
                     })
        <*> defineOption optionType_int
            (\o -> o { optionShortFlags  = ['l']
                     , optionLongFlags   = ["lambda"]
                     , optionDefault     = 8
                     , optionDescription = "Security parameter."
                     })
        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = ['v']
                     , optionLongFlags   = ["verbose"]
                     , optionDescription = "Be verbose."
                     })

main = runCommand $ \opts args -> do
    (c, ts) <- parseCirc <$> readFile (head args)
    let p = obfParams c
    if fakeMMap opts then do
        obf <- obfuscate (verbose opts) p fakeEncode (lambda opts) c
        return ()
    else do
        let (ObfParams {..}) = p
        mmap <- CLT.setup (verbose opts) (lambda opts) d nzs pows
        let enc x y ix = CLT.encode [x,y] ix mmap
        obf <- obfuscate (verbose opts) p enc (lambda opts) c
        return ()
