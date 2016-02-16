module Zim14.Serialize where

import Zim14.Obfuscate

import qualified CLT13 as CLT

import Control.Monad
import Control.Concurrent.ParallelIO
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Serialize as S

saveObfuscation :: FilePath -> Obfuscation CLT.Encoding -> IO ()
saveObfuscation dir obf = do
    createDirectoryIfMissing False dir
    forM_ (M.toList obf) $ \(k,v) -> do
        let fp = dir ++ "/" ++ show k
        BS.writeFile fp (S.encode v)

loadObfuscation :: FilePath -> IO (Obfuscation CLT.Encoding)
loadObfuscation dir = do
    fps <- listDirectory dir
    let obfs = filter ((/=) "mmap-" . take 5) fps
    let readEnc symstr = do
            let fp  = dir ++ "/" ++ symstr
                sym = read symstr
            bs <- BS.readFile fp
            case S.decode bs of
                Left err -> error err
                Right v  -> return (sym, v)
    obfs' <- parallelInterleaved $ map readEnc obfs
    return $ M.fromList obfs'

saveMMap :: FilePath -> CLT.PublicParams -> IO ()
saveMMap dir mmap = do
    createDirectoryIfMissing False dir
    let write name x = BS.writeFile (dir ++ "/mmap-" ++ name) (S.encode x)
    write "x0"  $ CLT.modulus    mmap
    write "pzt" $ CLT.zeroTester mmap
    write "nu"  $ CLT.threshold  mmap

loadMMap :: FilePath -> IO CLT.PublicParams
loadMMap dir = do
    let readSym name = do
            bs <- BS.readFile (dir ++ "/mmap-" ++ name)
            case S.decode bs of
                Left err -> error err
                Right v  -> return v
    x0  <- readSym "x0"
    pzt <- readSym "pzt"
    nu  <- readSym "nu"
    return $ CLT.PublicParams x0 pzt nu
