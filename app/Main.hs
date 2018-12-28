module Main where

import Data.Word
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader(runReaderT)
import Control.Concurrent        (threadDelay)
import I2C as I

main :: IO ()
main = withI2C i2cFilepath $ \fd -> do
  void $ flip runReaderT fd $
    runMaybeT $ do
      I.initialize i2cAddress
      liftIO $ putStrLn "start inserting"
      forever $ do
        ns <- I.tryGetIntsAt offset
        liftIO $ do
          reportStats ns
          threadDelay $ 500 * 1000


reportStats :: [Int] -> IO ()
reportStats ns =  print (maxVal, meanVal, minVal) where
  maxVal  = maximum ns 
  meanVal = sum ns `div` (length ns)
  minVal  = minimum ns

i2cFilepath :: FilePath
i2cFilepath  = "/dev/i2c-1"
i2cAddress  :: Int
i2cAddress   = 0x68
offset      :: Word8
offset       = 0x80
