module I2C (
    open, withFile, initIOControl, write1, read1, getDataAt,
    FileDesc, ErrorCode, MemoryAddress) where

import Data.Word
import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr

type FileDesc = CInt
type ErrorCode = Int
type MemoryAddress = Word8

open :: FilePath -> IO (Either () FileDesc)
open pathname = withCString pathname $ \path -> do
    fd <- c_open path o_RDWR
    return $ if fd < 0 then Left ()
             else Right $ fromIntegral fd

withFile :: FilePath -> (FileDesc -> IO ()) -> IO ()
withFile pathname action = do
    fd <- open pathname
    case fd of
        Left _ -> return ()
        Right fd' -> action fd'

initIOControl :: FileDesc -> Int -> IO (Either ErrorCode ())
initIOControl fd i2cAddress = do
    let address = fromIntegral i2cAddress
    ret <- c_ioctl fd i2C_SLAVE address
    return $ if ret < 0 then Left ret
             else Right ()

write1 :: FileDesc -> MemoryAddress -> IO (Either ErrorCode ())
write1 fd addr = withArray [fromIntegral addr] $ \arr -> do
    ret <- c_write fd arr 1
    return $ if ret /= 1 then Left ret
             else Right ()

read1 :: FileDesc -> MemoryAddress -> IO (Either ErrorCode Word8)
read1 fd addr = withArray [fromIntegral addr] $ \arr -> do
    ret <- c_read fd arr 1
    if ret /= 1 then
        return $ Left ret
    else do
        d:_ <- peekArray 1 arr
        return $ Right $ fromIntegral d

getDataAt :: FileDesc -> MemoryAddress -> IO (Either ErrorCode Word8)
getDataAt fd addr = do
    w <- write1 fd addr
    case w of 
        Left e -> return $ Left e
        Right _ -> read1 fd addr

o_RDWR :: CInt
o_RDWR = 2
i2C_SLAVE :: CUInt
i2C_SLAVE = 1795

foreign import ccall "open"  c_open  :: CString -> CInt -> IO FileDesc
foreign import ccall "ioctl" c_ioctl :: FileDesc -> CUInt -> CInt -> IO ErrorCode
foreign import ccall "write" c_write :: FileDesc -> Ptr CUChar -> CInt -> IO ErrorCode
foreign import ccall "read"  c_read  :: FileDesc -> Ptr CUChar -> CInt -> IO ErrorCode
