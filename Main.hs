{-# LANGUAGE RecordWildCards #-}

module Main where

import Zim14.Circuit
import Zim14.Circuit.Parser
import Zim14.Encoding.CLT13
import Zim14.Encoding.Fake
import Zim14.Index
import Zim14.Obfuscate
import Zim14.Serialize
import Zim14.Util (pr, readBitstring)

import CLT13.Util (forceM)
import qualified CLT13 as CLT

import Control.Concurrent.ParallelIO (stopGlobalPool)
import Control.Monad
import Options
import System.Directory
import System.Exit
import Text.Printf

data MainOptions = MainOptions { fake      :: Bool
                               , lambda    :: Int
                               , verbose   :: Bool
                               , fresh     :: Bool
                               , plaintext :: Bool
                               , input     :: Maybe String
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
                     , optionDescription = "Evaluate the plaintext circuit."
                     })
        <*> defineOption (optionType_maybe optionType_string)
            (\o -> o { optionShortFlags  = ['i']
                     , optionLongFlags   = ["input"]
                     , optionDescription = "Input as a bitstring."
                     })

main :: IO ()
main = runCommand $ \opts [fp] -> do
    (c, ts) <- parseCirc <$> readFile fp
    p       <- params (lambda opts) c

    when (verbose opts)   $ pr (show p)
    when (plaintext opts) $ evalPlaintextCircuit opts c ts

    if fake opts then
        evalFakeCircuit opts p c ts
    else
        evalObfuscatedCircuit fp opts p c ts

    stopGlobalPool -- cleanup

evalPlaintextCircuit :: MainOptions -> Circuit -> [TestCase] -> IO ()
evalPlaintextCircuit opts c ts = do
    case input opts of
        Nothing -> do
            pr "evaluating plaintext circuit tests"
            ok <- ensure plainEval c ts
            if ok then pr "ok" else pr "failed"
        Just str -> do
            when (length str /= ninputs c) $ do
                printf "incorrect size input! expected %d bits got %d\n"
                        (ninputs c) (length str)
                exitFailure
            printf "evaluating plaintext circuit with input x=%s: " str
            let res = plainEval c (readBitstring str)
            print res

evalFakeCircuit :: MainOptions -> Params -> Circuit -> [TestCase] -> IO ()
evalFakeCircuit opts p c ts = do
    let n = ninputs c
    obf <- obfuscate (verbose opts) p fakeEncode c
    when (verbose opts) $ pr "obfuscating"
    forceM obf
    case input opts of
        Nothing -> do
            pr "running tests"
            ok <- ensure (fakeEval obf p) c ts
            if ok then pr "ok" else pr "failed" >> exitFailure
        Just str -> do
            when (length str /= n) $ do
                printf "incorrect size input! expected %d bits got %d\n" n (length str)
                exitFailure
            pr ("evaluating on input x=" ++ str)
            let res = fakeEval obf p c (readBitstring str)
            pr ("result=" ++ show res)

evalObfuscatedCircuit :: FilePath -> MainOptions -> Params -> Circuit -> [TestCase] -> IO ()
evalObfuscatedCircuit fp opts p@(Params {..}) c ts = do
    -- obfuscate or load an existing obfuscation
    let dir = dirName fp λ
    (pp, obf) <- do
        exists <- doesDirectoryExist dir
        if not exists || fresh opts then do
            mmap <- CLT.setup (verbose opts) λ d (numIndices n) (topLevelCLTIndex c)
            let pp  = CLT.publicParams mmap
                enc = cltEncode mmap (indexer c)
            when (verbose opts) $ pr "obfuscating"
            obf <- obfuscate (verbose opts) p enc c
            saveMMap dir pp
            saveObfuscation dir obf
            return (pp, obf)
        else do
            when (verbose opts) $ pr "loading existing mmap"
            pp <- loadMMap dir
            when (verbose opts) $ pr "loading existing obfuscation"
            obf  <- loadObfuscation dir
            return (pp, obf)

    -- evaluate on the input or tests
    case input opts of
        Nothing -> do
            pr "running tests"
            ok <- ensure (cltEval obf pp) c ts
            if ok then pr "ok" else pr "failed" >> exitFailure
        Just str -> do
            when (length str /= n) $ do
                printf "incorrect size input! expected %d bits got %d\n" n (length str)
                exitFailure
            pr ("evaluating on input x=" ++ str)
            let res = cltEval obf pp c (readBitstring str)
            pr ("result=" ++ show res)

dirName :: FilePath -> Int -> FilePath
dirName fp λ = fp ++ "." ++ show λ
