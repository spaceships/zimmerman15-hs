module Zim14.Serialize where

import Zim14.Obfuscate

import qualified CLT13 as CLT

import Control.Monad
import Control.Concurrent.ParallelIO
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Serialize as S

saveObfuscation :: S.Serialize a => FilePath -> Obfuscation a -> IO ()
saveObfuscation dir obf = do
    createDirectoryIfMissing False dir
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
    obfs <- parallelInterleaved $ map readEnc obfs
    return $ M.fromList obfs

saveMMap :: FilePath -> CLT.PublicParams -> IO ()
saveMMap dir mmap = do
    createDirectoryIfMissing False dir
    let write name elem = BS.writeFile (dir ++ "/mmap-" ++ name) (S.encode elem)
    write "x0"  $ CLT.modulus    mmap
    write "pzt" $ CLT.zeroTester mmap
    write "nu"  $ CLT.threshold  mmap

loadMMap :: FilePath -> IO CLT.PublicParams
loadMMap dir = do
    fps <- listDirectory dir
    let read name = do
            bs <- BS.readFile (dir ++ "/mmap-" ++ name)
            case S.decode bs of
                Left err -> error err
                Right v  -> return v
    x0  <- read "x0"
    pzt <- read "pzt"
    nu  <- read "nu"
    return $ CLT.PublicParams x0 pzt nu
