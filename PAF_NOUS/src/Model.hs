
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import Carte (Carte)
import qualified Carte as C
import Environement (Environement)
import qualified Environement as E

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)


initGameState :: Int -> IO Environement
initGameState nbr = let carte = C.createCarte "HHHHHHHHHHHHHH"
                        listejoueurs = E.creerJoueur nbr in
                   E.smartConst_env carte listejoueurs

moveLeft :: GameState -> GameState
moveLeft gs@(GameState x _ s) 
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

-- check if a coordinates is inside the GameState or not
insideGameState :: Maybe (V2 Int) -> GameState -> Bool 
insideGameState coordinates gs@(GameState px py _) = 
  case coordinates of
    Just (V2 x y) -> px <= x && x <= (px + 100) && py <= y && y <= (py + 100)
    Nothing -> False

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
