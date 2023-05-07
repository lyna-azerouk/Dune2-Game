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
import Carte
import qualified Carte as C

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

main :: IO ()
main = do
    putStrLn "je suis la "
    putStrLn ( boolToString ( prop_positiveCoord_inv (Coord 2 1)))
