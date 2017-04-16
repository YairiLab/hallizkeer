import Data.Int
import Data.Word
import Data.Bits
import Control.Monad
import Control.Concurrent

import I2C

main :: IO ()
main = do
    withFile i2cFilepath $ \fd -> do
        Right _ <- initIOControl fd i2cAddress
        forever $ do
            ns <- forM [0..63] $ readValue fd
            let line = unwords $ map show ns
            putStrLn $ "data:" ++ line
            threadDelay $ 500 * 1000

readValue :: FileDesc -> Word8 -> IO Int16
readValue fd i = do
    Right low  <- getDataAt fd $ offset + 2*i
    Right high <- getDataAt fd $ offset + 2*i + 1
    let l = fromIntegral low
    let h = fromIntegral high
    return (h `shiftL` 8 + l)

i2cFilepath :: FilePath
i2cFilepath = "/dev/i2c-1"
i2cAddress :: Int
i2cAddress = 0x68
offset :: Word8
offset = 0x80
