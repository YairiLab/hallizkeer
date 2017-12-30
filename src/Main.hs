{-# LANGUAGE OverloadedStrings #-}
import Data.Word
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Concurrent
import Control.Applicative
import System.Posix.Syslog
import I2C

main :: IO ()
main = do
    withFile i2cFilepath $ \fd -> do
        Right _ <- initIOControl fd i2cAddress
        putStrLn "start inserting"
        forever $ do
            ns <- forM [0..63] $ readValue offset fd
            writeLog ns
            reportStats ns
            threadDelay $ 500 * 1000

writeLog :: [Int] -> IO ()
writeLog ns = writeLog' s
    where s = B.unwords $ (B.pack . show) <$> ns
writeLog' :: B.ByteString -> IO ()
writeLog' s = withSyslog defaultConfig $ \syslog -> do
    syslog facility Info s

reportStats :: [Int] -> IO ()
reportStats ns =  print (maxVal, meanVal, minVal)
    where maxVal  = maximum ns 
          meanVal = sum ns `div` (length ns)
          minVal  = minimum ns

facility :: Facility
facility = LOCAL5
i2cFilepath :: FilePath
i2cFilepath = "/dev/i2c-1"
i2cAddress :: Int
i2cAddress = 0x68
offset :: Word8
offset = 0x80
