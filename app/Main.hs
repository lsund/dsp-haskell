{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad.IO.Class                 (liftIO)
import           Data.ByteString                        (ByteString)
import           Data.ByteString.Builder                (floatLE,
                                                         toLazyByteString)
import           Data.ByteString.Lazy                   (toStrict)
import           Prelude                                hiding (writeFile)
import           System.Directory                       (createDirectoryIfMissing)
import           System.IO.Streams                      (Generator)
import qualified System.IO.Streams                      as Streams
import           System.IO.Streams.File
import           System.Process                         (runCommand)
import           Text.Printf                            (printf)

import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

import           Constants                              (outputFilePath,
                                                         plotFilePath,
                                                         sampleRate)
import           Note                                   (Note (..), note)
import           Types

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
  if i `mod` round (sampleRate * 2) == 0
     then do
       liftIO $ threadDelay 700000
       liftIO $ print $ "written" ++ show i
       generator (succ i) xs
  else
    generator (succ i) xs

doPlot :: [Pulse] -> IO ()
doPlot note = toFile def plotFilePath $ do
    layout_title .= "Note"
    setColors [opaque blue]
    plot (line "am" [zip [0,1..sampleRate] note])

main :: IO ()
main = do
  let theNote = note FSharp
  createDirectoryIfMissing True "data"
  doPlot theNote
  tid <- forkIO play
  is <- Streams.fromGenerator $ generator (round sampleRate) (concat (repeat theNote))
  putStrLn $ "Writing to" ++ outputFilePath
  withFileAsOutput  outputFilePath $ Streams.connect is

