{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set
import SDL
import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import qualified SDL
import Linear (V4(..))
import Keyboard (Keyboard)
import qualified Keyboard as K
import  Model as M

main :: IO ()
main = do
    s <- readFile "assets/Carte1.txt"
    initializeAll
    window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 365 520}
    renderer <- createRenderer window (-1) defaultRenderer
    -- initialisation de l'état du jeu
    let gameState = M.initGameState

    -- initialisation de l'état du clavier
    let kbd = K.createKeyboard
    -- initialisation de l'état de la souri


    --putStrLn (show (M.carte (Engine.modele engineState)))
