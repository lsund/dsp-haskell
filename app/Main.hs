{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Data.ByteString.Builder                (floatLE,
                                                         toLazyByteString)
import           Data.Foldable

import           Data.ByteString.Lazy                   (writeFile)
import           Prelude                                hiding (writeFile)
import           System.Directory                       (createDirectoryIfMissing)
import           System.Process                         (runCommand)
import           Text.Printf                            (printf)

import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

import           Constants                              (outputFilePath,
                                                         plotFilePath,
                                                         sampleRate)
import           Note                                   (Note (..), note)
import           Types

-- The options to ffplay are as follows
--
-- -loop 0     : loop infinitely
-- -autoexit   : exit on file end
-- -showmode 0 : windowless
-- -f f32le    : format 32-bit floats little endian
play :: IO ()
play = do
  _ <- runCommand $ printf "ffplay -loop 0 -autoexit -showmode 0 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

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
  putStrLn $ "Writing to" ++ outputFilePath
  writeFile outputFilePath $ toLazyByteString $ fold $ map floatLE theNote
  -- is <- Streams.fromGenerator $ generator (round sampleRate) (concat (repeat theNote))
  -- withFileAsOutput  outputFilePath $ Streams.connect is

