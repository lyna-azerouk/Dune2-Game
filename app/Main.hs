{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Foreign.C.Types (CInt (..) )


import SDL

import Carte
import qualified Carte as C


main :: IO ()
main = do
  initializeAll
  window <- createWindow "Dune2" $ defaultWindow { windowInitialSize = V2 750 750 }
  renderer <- createRenderer window (-1) defaultRenderer
