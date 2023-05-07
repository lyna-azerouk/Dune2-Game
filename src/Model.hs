
module Model where

--import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K


import Data.Int (Int32)
import qualified Data.Int as I

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)


-- FAUT DEFINIR LES REGLES DU JEU

-- Permet d'extraire les coordonnees du personnage
gameStateToCoord :: GameState -> Coordonnee
gameStateToCoord (GameState x y s) = M.Coordonnee  (toInteger x) (toInteger y)

--inicialisation  avec speed===4
initGameState :: GameState
initGameState = GameState 0 0 4

--MouvementS   
moveLeft :: GameState -> GameState
moveLeft gs@(GameState x _ s)    --MOVE LEFT WITH VALUE "s"
  | x > 0 = gs{persoX = x - s}
  | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState x _ s) 
  | x < 540 = gs{persoX = x + s}
  | otherwise = gs
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ y s) 
  | y > 0 = gs{persoY = y - s}
  | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ y s) 
  | y < 380 = gs{persoY = y + s}
  | otherwise = gs



gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)

  in modif gstate