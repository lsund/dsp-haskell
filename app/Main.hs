{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (createDirectoryIfMissing)
import Prelude hiding (writeFile)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString, floatLE)
import System.Process (runCommand)
import Text.Printf (printf)
import System.IO.Streams (Generator)
import qualified System.IO.Streams as Streams
import System.IO.Streams.File
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

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
  if i `mod` round (sampleRate * 2) == 0
     then do
       liftIO $ threadDelay 700000
       liftIO $ print $ "written" ++ show i
       generator (succ i) xs
  else
    generator (succ i) xs

doPlot :: [Pulse] -> IO ()
doPlot note = toFile def "./data/plot.png" $ do
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

