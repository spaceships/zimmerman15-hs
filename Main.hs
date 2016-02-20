{-# LANGUAGE RecordWildCards #-}

module Main where

import Zim14.Circuit
import Zim14.Circuit.Arbitrary
import Zim14.Circuit.Parser
import Zim14.Encoding.CLT13
import Zim14.Encoding.Fake
import Zim14.Index
import Zim14.Obfuscate
import Zim14.Serialize
import Zim14.Util (pr, readBitstring, num2Bits)

import CLT13.Rand
import CLT13.Util (forceM)
import qualified CLT13 as CLT

import Control.Concurrent.ParallelIO (stopGlobalPool)
import Control.Monad
import Debug.Trace
import Options
import System.Exit
import System.Posix
import Text.Printf

data MainOptions = MainOptions { fake      :: Bool
                               , lambda    :: Int
                               , verbose   :: Bool
                               , fresh     :: Bool
                               , plaintext :: Bool
                               , input     :: Maybe String
                               , gentests  :: Maybe Int
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
        <*> defineOption (optionType_maybe optionType_int)
            (\o -> o { optionLongFlags   = ["gentests"]
                     , optionDescription = "Generate tests from the plaintext eval."
                     })

main :: IO ()
main = runCommand $ \opts [fp] -> do
    (c, ts) <- parseCirc <$> readFile fp
    ts' <- case gentests opts of
        Nothing -> return ts
        Just i  -> replicateM i (genTest c)
    when (plaintext opts) $ evalPlaintextCircuit opts c ts'
    let λ = lambda opts
    if fake opts then
        evalFakeCircuit opts λ c ts'
    else
        evalObfuscatedCircuit fp opts λ c ts'
    stopGlobalPool -- cleanup

evalPlaintextCircuit :: MainOptions -> Circuit -> [TestCase] -> IO ()
evalPlaintextCircuit opts c ts = do
    case input opts of
        Nothing -> do
            pr "evaluating plaintext circuit tests"
            ok <- ensure (verbose opts) plainEvalIO c ts
            if ok then pr "ok" else pr "failed"
        Just str -> do
            when (length str /= ninputs c) $ do
                printf "incorrect size input! expected %d bits got %d\n"
                        (ninputs c) (length str)
                exitFailure
            printf "evaluating plaintext circuit with input x=%s: " str
            let res = plainEval c (readBitstring str)
            print res

evalFakeCircuit :: MainOptions -> Int -> Circuit -> [TestCase] -> IO ()
evalFakeCircuit opts λ c ts = do
    let n = ninputs c
    [n_ev, n_chk] <- randIO (randPrimes 2 λ)
    let p = Params n_ev n_chk
    obf <- obfuscate (verbose opts) p fakeEncode c
    when (verbose opts) $ pr "obfuscating"
    forceM obf
    case input opts of
        Nothing -> do
            pr "running tests"
            ok <- ensure (verbose opts) (fakeEval obf p) c ts
            if ok then pr "ok" else pr "failed" >> exitFailure
        Just str -> do
            when (length str /= n) $ do
                printf "incorrect size input! expected %d bits got %d\n" n (length str)
                exitFailure
            pr ("evaluating on input x=" ++ str)
            res <- fakeEval obf p c (readBitstring str)
            pr ("result=" ++ show res)

evalObfuscatedCircuit :: FilePath -> MainOptions -> Int -> Circuit -> [TestCase] -> IO ()
evalObfuscatedCircuit fp opts λ c ts = do
    -- obfuscate or load an existing obfuscation
    let dir = dirName fp λ
        n   = ninputs c
        d   = depth c
    (pp, obf) <- do
        exists <- fileExist dir
        if not exists || fresh opts then do
            mmap <- CLT.setup (verbose opts) (λ+d) (getKappa c) (numIndices c) (topLevelCLTIndex c)
            let pp  = CLT.publicParams mmap
                enc = cltEncode mmap (indexer c)
                n_ev  = CLT.gs mmap !! 0
                n_chk = CLT.gs mmap !! 1
            when (verbose opts) $ pr "obfuscating"
            obf <- obfuscate (verbose opts) (Params n_ev n_chk) enc c
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
            ok <- ensure (verbose opts) (cltEval obf pp) c ts
            if ok then pr "ok" else pr "failed" >> exitFailure
        Just str -> do
            when (length str /= n) $ do
                printf "incorrect size input! expected %d bits got %d\n" n (length str)
                exitFailure
            pr ("evaluating on input x=" ++ str)
            res <- cltEval obf pp c (readBitstring str)
            pr ("result=" ++ show res)

dirName :: FilePath -> Int -> FilePath
dirName fp λ = fp ++ "." ++ show λ

genTest :: Circuit -> IO TestCase
genTest c = do
    inp <- num2Bits (ninputs c) <$> randIO (randInteger (ninputs c))
    return (reverse inp, plainEval c inp)

getKappa :: Circuit -> Int
getKappa c = trace ("Δ=" ++ show δ) $ δ + 2*n + n*(2*n-1)
  where
    n = ninputs c
    δ = ydeg c + sum (xdegs c)
