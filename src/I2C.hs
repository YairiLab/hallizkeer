module I2C (withFile, initIOControl, readValue,
    FileDesc, ErrorCode, MemoryAddress) where

import Data.Word
import Data.Bits
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class
import I2CLow

withFile :: FilePath -> (FileDesc -> IO ()) -> IO ()
withFile pathname action = do
    fd <- open pathname
    case fd of
        Left _    -> return ()
        Right fd' -> action fd'

getDataAt :: FileDesc -> MemoryAddress -> IO (Either ErrorCode Word8)
getDataAt fd addr = do
    w <- write1 fd addr
    case w of 
        Left e  -> return $ Left e
        Right _ -> read1 fd addr

readValue :: Word8 -> FileDesc -> Word8 -> IO Int
readValue offset fd i = do
    Right low  <- getDataAt fd $ offset + 2*i
    Right high <- getDataAt fd $ offset + 2*i + 1
    let [l, h] = map fromIntegral [low, high]
    return (h `shiftL` 8 + l)
