{-# LANGUAGE ScopedTypeVariables #-}

module Zim14.Serialize where

import Zim14.Obfuscate (Obfuscation)

import qualified CLT13 as CLT

import Control.Monad.IfElse (whenM)
import Control.Monad
import Control.Concurrent.ParallelIO
import Control.Exception
import System.Posix
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Serialize as S

createDirectoryIfMissing :: FilePath -> IO ()
createDirectoryIfMissing dir = do
    whenM (not <$> fileExist dir) $ do
        createDirectory dir stdFileMode

listDirectory :: FilePath -> IO [FilePath]
listDirectory dir = do
    ds    <- openDirStream dir
    files <- getFile ds
    closeDirStream ds
    return files
  where
    getFile s = do
        maybeF <- catch (Just <$> readDirStream s)
                        (\(_ :: IOException) -> return Nothing)
        case maybeF of
            Nothing -> return []
            Just f  -> do
                rest <- getFile s
                return (f : rest)

saveObfuscation :: S.Serialize a => FilePath -> Obfuscation a -> IO ()
saveObfuscation dir obf = do
    createDirectoryIfMissing dir
    forM_ (M.toList obf) $ \(k,v) -> do
        let fp = dir ++ "/" ++ show k
        BS.writeFile fp (S.encode v)

loadObfuscation :: S.Serialize a => FilePath -> IO (Obfuscation a)
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
    createDirectoryIfMissing dir
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
