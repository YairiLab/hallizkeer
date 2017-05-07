{-# LANGUAGE OverloadedStrings #-}
import Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Concurrent

import System.Posix.Syslog
import I2C

main :: IO ()
main = do
    withFile i2cFilepath $ \fd -> do
        Right _ <- initIOControl fd i2cAddress
        putStrLn "start inserting"
        forever $ do
            ns <- forM [0..63] $ readValue fd
            writeLog ns
            reportProgress ns
            threadDelay $ 500 * 1000

readValue :: FileDesc -> Word8 -> IO Int
readValue fd i = do
    Right low  <- getDataAt fd $ offset + 2*i
    Right high <- getDataAt fd $ offset + 2*i + 1
    let low' = fromIntegral low
    let high' = fromIntegral high
    return (high' `shiftL` 8 + low')

writeLog :: [Int] -> IO ()
writeLog ns = writeLog' s
    where s = B.unwords [B.pack $ show n | n<-ns]
writeLog' :: B.ByteString -> IO ()
writeLog' s = withSyslog defaultConfig $ \syslog -> do
    syslog facility Info s

reportProgress :: [Int] -> IO ()
reportProgress ns =  putStrLn $ show mean
    where mean = sum ns `div` (length ns)

facility :: Facility
facility = LOCAL5
i2cFilepath :: FilePath
i2cFilepath = "/dev/i2c-1"
i2cAddress :: Int
i2cAddress = 0x68
offset :: Word8
offset = 0x80
