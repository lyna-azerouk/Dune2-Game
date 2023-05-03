
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K


import Data.Int (Int32)
import qualified Data.Int as I

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)


-- FAUT DEFINIR LES REGLES DU JEU 