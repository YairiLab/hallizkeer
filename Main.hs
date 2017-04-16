{-# LANGUAGE OverloadedStrings #-}
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as B

import Database.PostgreSQL.LibPQ
import Control.Monad
import Control.Concurrent

import I2C

main :: IO ()
main = do
    conn <- connectToPostgres connectionString

    withFile i2cFilepath $ \fd -> do
        Right _ <- initIOControl fd i2cAddress
        putStrLn "start inserting"
        forever $ do
            ns <- forM [0..63] $ readValue fd
            insertData conn ns
            putStrLn $ show $ sum ns `div` (length ns)
            threadDelay $ 2000 * 1000

connectToPostgres :: B.ByteString -> IO Connection
connectToPostgres connStr = do
    putStr "start Postgres connection..."
    conn <- connectdb connStr
    errorMessage conn >>= \e -> case e of
        Just "" -> putStrLn "ok"
        Just e -> B.putStrLn e
        _ -> error "fatal error!"
    return conn

insertData :: Connection -> [Int] -> IO (Maybe Result)
insertData conn ns = exec conn stmt
    where col = B.pack $ unwords $ map show ns
          stmt = B.concat ["INSERT INTO sensor_data (data_array) VALUES('", col, "');"]

readValue :: FileDesc -> Word8 -> IO Int
readValue fd i = do
    Right low  <- getDataAt fd $ offset + 2*i
    Right high <- getDataAt fd $ offset + 2*i + 1
    let l = fromIntegral low
    let h = fromIntegral high
    return (h `shiftL` 8 + l)

connectionString :: B.ByteString
connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=testdb"

i2cFilepath :: FilePath
i2cFilepath = "/dev/i2c-1"
i2cAddress :: Int
i2cAddress = 0x68
offset :: Word8
offset = 0x80
