module I2C (
    withFile, initialize, readValues,
    FileDesc, ErrorCode, MemoryAddress,
    Word8,
) where

import Data.Bits                  (shiftL)
import Control.Monad              (forM)
import Control.Monad.Trans.Maybe  (MaybeT(MaybeT))
import Control.Monad.Trans.Reader (ReaderT(ReaderT), ask)
import Control.Monad.IO.Class     (liftIO)
import I2CLow

type DescReaderIO = ReaderT FileDesc IO

withFile :: FilePath -> (FileDesc -> IO ()) -> IO ()
withFile pathname actUsing = do
    fd <- open pathname
    case fd of
        Left _    -> return ()
        Right fd' -> actUsing fd'

initialize :: Int -> MaybeT DescReaderIO ()
initialize addr = MaybeT (initIOControl' addr)

readValues :: Word8 -> MaybeT DescReaderIO [Int]
readValues offset = forM [0..63] (readValue offset)

readValue :: MemoryAddress -> Int -> MaybeT DescReaderIO Int
readValue offset i = do
    let j = offset + 2*fromIntegral i
    [low, high]  <- forM [j, j+1] $ \k -> do
        x <- readByteAt k
        return $ fromIntegral x
    return $ (high`shiftL`8 + low)

readByteAt :: MemoryAddress -> MaybeT DescReaderIO Word8
readByteAt addr = do
    maybeWrite addr
    maybeRead  addr


maybeWrite :: MemoryAddress -> MaybeT DescReaderIO ()
maybeWrite addr = MaybeT (write1' addr)
maybeRead  :: MemoryAddress -> MaybeT DescReaderIO Word8
maybeRead  addr = MaybeT (read1' addr)

write1' :: MemoryAddress -> DescReaderIO (Maybe ())
write1' addr = do
    fd <- ask
    r <- liftIO $ write1 fd addr
    return $ case r of
        Left _  -> Nothing
	Right _ -> Just ()

read1' :: MemoryAddress -> DescReaderIO (Maybe Word8)
read1' addr = do
    fd <- ask
    r <- liftIO $ read1 fd addr
    return $ case r of
        Left _  -> Nothing
	Right x -> Just x

initIOControl' :: Int -> DescReaderIO (Maybe ())
initIOControl' addr = do
    fd <- ask
    r <- liftIO $ initIOControl fd addr
    return $ case r of
        Left _  -> Nothing
        Right _ -> Just ()
