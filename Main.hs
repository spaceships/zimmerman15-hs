{-# LANGUAGE RecordWildCards #-}

module Main where

import Zim14.Circuit
import Zim14.Circuit.Parser
import Zim14.FakeMMap
import Zim14.Index
import Zim14.Obfuscate
import Zim14.Serialize

import CLT13.Util (forceM)
import qualified CLT13 as CLT

import Control.Concurrent.ParallelIO (stopGlobalPool)
import Control.Monad
import Options
import System.Directory

data MainOptions = MainOptions { fakeMMap :: Bool
                               , lambda   :: Int
                               , verbose  :: Bool
                               , fresh    :: Bool
                               }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> defineOption optionType_bool
            (\o -> o { optionLongFlags   = ["fake"]
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
        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = ['f']
                     , optionLongFlags   = ["fresh"]
                     , optionDescription = "Generate a fresh mmap."
                     })

main = runCommand $ \opts args -> do
    let [fp] = args
    (c, ts) <- parseCirc <$> readFile fp
    let p   = obfParams c
        dir = dirName fp (lambda opts)
    if fakeMMap opts then do
        let dir' = dir ++ ".fake"
        exists <- doesDirectoryExist dir'
        obf <- obfuscate p fakeEncode (lambda opts) c
        forceM obf
        return ()
    else do
        exists <- doesDirectoryExist dir
        (pp, obf) <-
            if not exists || fresh opts then do
                let (ObfParams {..}) = p
                mmap <- CLT.setup (verbose opts) (lambda opts) d nzs pows
                let enc x y i = CLT.encode [x,y] (ix i) mmap
                when (verbose opts) $ putStrLn "obfuscating"
                obf <- obfuscate p enc (lambda opts) c
                forceM obf
                let pp = CLT.publicParams mmap
                saveMMap dir pp
                saveObfuscation dir obf
                return (pp, obf)
            else do
                when (verbose opts) $ putStrLn "loading existing mmap"
                pp <- loadMMap dir
                when (verbose opts) $ putStrLn "loading existing obfuscation"
                obf  <- loadObfuscation dir
                return (pp, obf)
        forceM obf
    stopGlobalPool

dirName :: FilePath -> Int -> FilePath
dirName fp λ = fp ++ "." ++ show λ
