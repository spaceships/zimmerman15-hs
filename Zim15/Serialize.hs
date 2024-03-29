{-# LANGUAGE ScopedTypeVariables #-}

module Zim15.Serialize where

import Zim15.Obfuscate (Obfuscation)

import qualified CLT13 as CLT

import Control.Monad.IfElse (whenM)
import Control.Monad
import Control.Concurrent.ParallelIO
import System.Posix
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Serialize as S

createDirectoryIfMissing :: FilePath -> IO ()
createDirectoryIfMissing dir = do
    whenM (not <$> fileExist dir) $ do
        createDirectory dir (unionFileModes stdFileMode ownerExecuteMode)

listDirectory :: FilePath -> IO [FilePath]
listDirectory dir = do
    ds    <- openDirStream dir
    files <- getAllFiles ds
    closeDirStream ds
    return files
  where
    getAllFiles s = do
        f <- readDirStream s
        case f of
            ""   -> return []
            "."  -> getAllFiles s
            ".." -> getAllFiles s
            x    -> do
                rest <- getAllFiles s
                return (x : rest)

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
    M.fromList <$> (parallelInterleaved (map readEnc obfs))

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
