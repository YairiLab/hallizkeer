module I2C (
    withI2C, initialize, tryGetIntsAt,
    C.FileDesc, C.ErrorCode, C.MemAddress) where

import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import qualified Control.Monad.Trans.Maybe  as M
import qualified Control.Monad.Trans.Reader as R
import qualified I2C.C as C

type DescReaderIO = R.ReaderT C.FileDesc IO

withI2C :: FilePath -> (C.FileDesc -> IO ()) -> IO ()
withI2C pathname action = bracket (C.open pathname)
  (\(Right fd) -> action fd) $
  \fd -> case fd of
    Left _   -> return ()
    Right fd -> void $ C.close fd

initialize :: Int -> M.MaybeT DescReaderIO ()
initialize addr = M.MaybeT (initIOControl addr)

tryGetIntsAt :: Word8 -> M.MaybeT DescReaderIO [Int]
tryGetIntsAt offset = forM [0..63] (tryGetIntAt offset)

tryGetIntAt :: C.MemAddress -> Int -> M.MaybeT DescReaderIO Int
tryGetIntAt offset i = do
  let j = offset + 2*fromIntegral i
  [low, high]  <- forM [j, j+1] $ \k -> do
    fromIntegral <$> tryGetByteAt k
  return $ (high`shiftL`8 + low)

tryGetByteAt :: C.MemAddress -> M.MaybeT DescReaderIO Word8
tryGetByteAt addr = do
  maybeWrite addr
  maybeRead  addr

maybeRead  :: C.MemAddress -> M.MaybeT DescReaderIO Word8
maybeRead  addr = M.MaybeT (readAt addr)
maybeWrite :: C.MemAddress -> M.MaybeT DescReaderIO ()
maybeWrite addr = M.MaybeT (writeAt addr)

readAt :: C.MemAddress -> DescReaderIO (Maybe Word8)
readAt addr = do
  fd <- R.ask
  r <- liftIO $ C.readAt fd addr
  return $ case r of
    Right x -> Just x
    _       -> Nothing

writeAt :: C.MemAddress -> DescReaderIO (Maybe ())
writeAt addr = do
  fd <- R.ask
  r <- liftIO $ C.writeAt fd addr
  return $ case r of
    Right _ -> Just ()
    _       -> Nothing

initIOControl :: Int -> DescReaderIO (Maybe ())
initIOControl addr = do
  fd <- R.ask
  r <- liftIO $ C.initIOControl fd addr
  return $ case r of
    Right _ -> Just ()
    _       -> Nothing
