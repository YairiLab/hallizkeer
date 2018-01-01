module Logger (writeLog) where

import Control.Applicative       ((<$>))
import qualified Data.ByteString.Char8 as B
import System.Posix.Syslog (Facility(LOCAL5), Priority(Info), withSyslog, defaultConfig)

writeLog :: [Int] -> IO ()
writeLog ns = writeLog' s
    where s = B.unwords $ (B.pack . show) <$> ns
writeLog' :: B.ByteString -> IO ()
writeLog' s = withSyslog defaultConfig $ \syslog -> do
    syslog facility Info s

facility    :: Facility
facility     = LOCAL5
