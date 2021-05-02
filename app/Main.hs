{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy (writeFile, toStrict)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString, floatLE)
import System.Process (runCommand)
import Text.Printf (printf)
import System.IO.Streams (InputStream, Generator)
import qualified System.IO.Streams as Streams
import System.IO.Streams.File
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Types
import Note (note, Note(..))
import Constants (outputFilePath, sampleRate)

toPCM :: Pulse -> ByteString
toPCM = toStrict . toLazyByteString . floatLE

play :: IO ()
play = do
  threadDelay 2000000
  putStrLn "Starting playback"
  _ <- runCommand $ printf "ffplay -autoexit -showmode 0 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

-- TODO Add real time key listener for stop writing stream
generator :: Int -> [Pulse] -> Generator ByteString ()
generator i [] = Streams.yield $ toPCM 0.0
generator i (x : xs) = do
  Streams.yield $ toPCM x
  if i `mod` round sampleRate == 0
     then do
       liftIO $ threadDelay 700000
       liftIO $ print $ "written" ++ show i
       generator (succ i) xs
  else
    generator (succ i) xs

main :: IO ()
main = do
  tid <- forkIO play
  is <- Streams.fromGenerator $ generator (round sampleRate) (concat (repeat (note FSharp)))
  putStrLn $ "Writing to" ++ outputFilePath
  withFileAsOutput  outputFilePath $ Streams.connect is

