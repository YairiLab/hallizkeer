module I2C.C (
    open, close, initIOControl, readAt, writeAt,
    FileDesc, ErrorCode, MemAddress) where

import Control.Applicative
import Data.Word
import qualified Foreign.C.Types as C
import qualified Foreign.C.String as C
import qualified Foreign.Marshal.Array as C
import qualified Foreign.Ptr as C

newtype FileDesc  = FileDesc C.CInt
newtype ErrorCode = ErrorCode C.CInt deriving Show
type MemAddress   = Word8

open :: FilePath -> IO (Either ErrorCode FileDesc)
open pathname = C.withCString pathname $ \path -> do
  fd@(FileDesc i) <- c_open path o_RDWR
  return $ if i < 0
           then Left  $ ErrorCode i
           else Right $ fd

close :: FileDesc -> IO (Either ErrorCode ())
close fd = do
  e@(ErrorCode c) <- c_close fd
  return $ if c /= 0
           then Left e
           else Right ()


initIOControl :: FileDesc -> Int -> IO (Either ErrorCode ())
initIOControl fd i2cAddress = do
  let address = fromIntegral i2cAddress
  e@(ErrorCode c) <- c_ioctl fd i2C_SLAVE address
  return $ if c /= 0
           then Left e
           else Right ()

readAt :: FileDesc -> MemAddress -> IO (Either ErrorCode Word8)
readAt fd addr = C.withArray [fromIntegral addr] $ \arr -> do
  e@(ErrorCode c) <- c_read fd arr 1
  if c /= 1
  then return $ Left e
  else do
    d:_ <- C.peekArray 1 arr
    return $ Right $ fromIntegral d

writeAt :: FileDesc -> MemAddress -> IO (Either ErrorCode ())
writeAt fd addr = C.withArray [fromIntegral addr] $ \arr -> do
  e@(ErrorCode c) <- c_write fd arr 1
  return $ if c /= 1
           then Left e
           else Right ()

o_RDWR :: C.CInt
o_RDWR = 2
i2C_SLAVE :: C.CUInt
i2C_SLAVE = 1795

foreign import ccall "open"  c_open  :: C.CString -> C.CInt -> IO FileDesc
foreign import ccall "close" c_close :: FileDesc -> IO ErrorCode
foreign import ccall "ioctl" c_ioctl :: FileDesc -> C.CUInt -> C.CInt -> IO ErrorCode
foreign import ccall "read"  c_read  :: FileDesc -> C.Ptr C.CUChar -> C.CInt -> IO ErrorCode
foreign import ccall "write" c_write :: FileDesc -> C.Ptr C.CUChar -> C.CInt -> IO ErrorCode
