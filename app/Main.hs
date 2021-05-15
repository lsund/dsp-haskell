{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Data.ByteString.Builder                (doubleLE,
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
import           Note                                   (Note (..), toPulses)
import           Types

-- The options to ffplay are as follows
--
-- -loop 0     : loop infinitely
-- -autoexit   : exit on file end
-- -showmode 0 : windowless
-- -f f32le    : format 32-bit floats little endian
play :: IO ()
play = do
  _ <- runCommand $ printf "ffplay -loop 0 -autoexit -showmode 0 -f f64le -ar %f %s" sampleRate outputFilePath
  return ()

renderPlot :: Note -> [Pulse] -> IO ()
renderPlot note pulses = toFile def plotFilePath $ do
    layout_title .= show note
    setColors [opaque blue]
    plot (line (show note) [zip [0,1..sampleRate] pulses])

main :: IO ()
main = do
  let pulses = toPulses FSharp (-4)
  createDirectoryIfMissing True "data"
  renderPlot FSharp pulses
  writeFile outputFilePath $ toLazyByteString $ foldMap doubleLE pulses
  tid <- forkIO play
  return ()

