import Control.Monad             (forever)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader(runReaderT)
import Control.Concurrent        (threadDelay)
import I2C
import Logger

main :: IO ()
main = withFile i2cFilepath $ \fd -> do
    flip runReaderT fd $ do
        runMaybeT $ do
            initialize i2cAddress
            liftIO $ putStrLn "start inserting"
            forever $ do
                ns <- readValues offset
                liftIO $ do
                    writeLog ns
                    reportStats ns
                    threadDelay $ 500 * 1000
    return ()


reportStats :: [Int] -> IO ()
reportStats ns =  print (maxVal, meanVal, minVal)
    where maxVal  = maximum ns 
          meanVal = sum ns `div` (length ns)
          minVal  = minimum ns

i2cFilepath :: FilePath
i2cFilepath  = "/dev/i2c-1"
i2cAddress  :: Int
i2cAddress   = 0x68
offset      :: Word8
offset       = 0x80
