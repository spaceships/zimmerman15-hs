{-# LANGUAGE RecordWildCards #-}

module Main where

import Zim14.Circuit
import Zim14.Circuit.Parser
import Zim14.FakeMMap
import Zim14.Index
import Zim14.Obfuscate
import Zim14.Serialize
import Zim14.Util (pr)

import CLT13.Util (forceM)
import qualified CLT13 as CLT

import Control.Concurrent.ParallelIO (stopGlobalPool)
import Control.Monad
import Options
import System.Directory
import System.IO

data MainOptions = MainOptions { fakeMMap  :: Bool
                               , lambda    :: Int
                               , verbose   :: Bool
                               , fresh     :: Bool
                               , plaintext :: Bool
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
        <*> defineOption optionType_bool
            (\o -> o { optionLongFlags   = ["plaintext"]
                     , optionDescription = "Run the tests on the plaintext circuit."
                     })

main = runCommand $ \opts args -> do
    let [fp] = args
    (c, ts) <- parseCirc <$> readFile fp
    when (plaintext opts) $ do
        pr "evaluating plaintext tests"
        ok <- ensure plainEval c ts
        if ok then pr "ok" else pr "failed"
        return ()
    let λ   = lambda opts
        dir = dirName fp λ
    p <- params λ c
    if fakeMMap opts then do
        let dir' = dir ++ ".fake"
        exists <- doesDirectoryExist dir'
        obf <- obfuscate p fakeEncode λ c
        when (verbose opts) $ pr "obfuscating"
        forceM obf
        when (verbose opts) $ pr "evaluating"
        ensure (fakeEval obf p) c ts
        return ()
    else do
        exists <- doesDirectoryExist dir
        (pp, obf) <-
            if not exists || fresh opts then do
                let (Params {..}) = p
                    ix       = indexer n
                    topLevel = topLevelCLTIndex ix n (ydeg c) (xdegs c)
                mmap <- CLT.setup (verbose opts) λ d (nindices n) topLevel
                let enc x y i = CLT.encode [x,y] (ix i) mmap
                when (verbose opts) $ pr "obfuscating"
                obf <- obfuscate p enc λ c
                forceM obf
                let pp = CLT.publicParams mmap
                saveMMap dir pp
                saveObfuscation dir obf
                return (pp, obf)
            else do
                when (verbose opts) $ pr "loading existing mmap"
                pp <- loadMMap dir
                when (verbose opts) $ pr "loading existing obfuscation"
                obf  <- loadObfuscation dir
                return (pp, obf)
        forceM obf
    stopGlobalPool

dirName :: FilePath -> Int -> FilePath
dirName fp λ = fp ++ "." ++ show λ
