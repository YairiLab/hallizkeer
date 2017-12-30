module I2C (withFile, initialize, readValues,
    FileDesc, ErrorCode, MemoryAddress) where

import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import I2CLow

withFile :: FilePath -> (FileDesc -> IO ()) -> IO ()
withFile pathname actUsing = do
    fd <- open pathname
    case fd of
        Left _    -> return ()
        Right fd' -> actUsing fd'

initialize :: Int -> ReaderT FileDesc IO (Maybe ())
initialize addr = do
    fd <- ask
    r <- liftIO $ initIOControl fd addr
    case r of
        Left _  -> return Nothing
        Right _ -> return $ Just ()


readValues :: Word8 -> ReaderT FileDesc IO ([Maybe Int])
readValues offset = forM [0..63] (readValue offset)

readValue :: Word8 -> Word8 -> ReaderT FileDesc IO (Maybe Int)
readValue offset i = do
    Just low  <- getDataAt (offset + 2*i)
    Just high <- getDataAt (offset + 2*i+1)
    let [l, h] = map fromIntegral [low, high]
    return $ Just (h `shiftL` 8 + l)

getDataAt :: MemoryAddress -> ReaderT FileDesc IO (Maybe Word8)
getDataAt addr = do
    fd <- ask
    r1 <- liftIO $ write1 fd addr
    case r1 of
        Left _  -> return Nothing
	Right _ -> do
            r2 <- liftIO $ read1 fd addr
	    case r2 of
                Left _ -> return Nothing
		Right x -> return $ Just x

