module I2C (
    withFile, initialize, readValues,
    FileDesc, ErrorCode, MemoryAddress,
    Word8,
    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.Trans.Reader,
    module Control.Monad.IO.Class) where

import Data.Bits
import Control.Applicative
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
    r  <- liftIO $ initIOControl fd addr
    return $ rightToMaybe r

readValues :: Word8 -> ReaderT FileDesc IO ([Maybe Int])
readValues offset = forM [0..63] (readValue offset)

readValue :: Word8 -> Word8 -> ReaderT FileDesc IO (Maybe Int)
readValue offset i = do
    Just low  <- readByteAt (offset + 2*i)
    Just high <- readByteAt (offset + 2*i+1)
    let [l, h] = map fromIntegral [low, high]
    return $ Just (h `shiftL` 8 + l)

readByteAt :: MemoryAddress -> ReaderT FileDesc IO (Maybe Word8)
readByteAt addr = do
    fd <- ask
    r1 <- liftIO $ write1 fd addr
    case r1 of
        Left _ -> return Nothing
	Right _ -> do
	    r2 <- liftIO $ read1 fd addr
            return $ rightToMaybe r2

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right x) = Just x
