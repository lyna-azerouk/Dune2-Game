{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M
import GHC.Base (when)

import Environement (Environement)
import qualified Environement as E

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

getTextureIdForBatiment::E.TypeBatiment -> TextureId 
getTextureIdForBatiment batid = 
  case batid of 
    E.QG _ -> (TextureId "qg")
    E.Raffinerie _ -> (TextureId "raffinerie")
    E.Usine _ _ _ _ -> (TextureId "usine")
    E.Centrale _ -> (TextureId "centrale")


getFilePathForBatiment::E.TypeBatiment -> FilePath
getFilePathForBatiment batid = 
  case batid of 
    E.QG _ -> "assets/virus.bmp"
    E.Raffinerie _ -> "assets/virus.bmp"
    E.Usine _ _ _ _ -> "assets/virus.bmp"
    E.Centrale _ -> "assets/virus.bmp"


getSpriteIdForBatiment::E.TypeBatiment -> SpriteId 
getSpriteIdForBatiment batid = 
  case batid of 
    E.QG _ -> (SpriteId "qg")
    E.Raffinerie _ -> (SpriteId "raffinerie")
    E.Usine _ _ _ _ -> (SpriteId "usine")
    E.Centrale _ -> (SpriteId "centrale")

loadBatiment:: Renderer -> E.Batiment ->TextureMap -> SpriteMap-> IO (TextureMap,SpriteMap)
loadBatiment renderer batiment tmap smap = do 
  tmap' <- TM.loadTexture renderer (getFilePathForBatiment (E.typeb batiment)) (getTextureIdForBatiment (E.typeb batiment)) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (getTextureIdForBatiment (E.typeb batiment)) (S.mkArea 0 0 32 32)
  let smap'=SM.addSprite (getSpriteIdForBatiment (E.typeb batiment)) sprite smap

  return (tmap',smap')

getTextureIdForUnite::E.TypeUnite-> TextureId 
getTextureIdForUnite uniid = 
  case uniid of 
    E.Collecteur _ _ -> (TextureId "collecteur")
    E.Combattant-> (TextureId "combattant")


getFilePathForUnite::E.TypeUnite -> FilePath
getFilePathForUnite uniid = 
  case uniid of 
    E.Collecteur _ _ -> "assets/virus.bmp"
    E.Combattant-> "assets/virus.bmp"


getSpriteIdForUnite::E.TypeUnite -> SpriteId 
getSpriteIdForUnite uniid = 
   case uniid of 
    E.Collecteur _ _ -> (SpriteId "collecteur")
    E.Combattant-> (SpriteId "combattant")


loadUnite:: Renderer -> E.Unite->TextureMap -> SpriteMap-> IO (TextureMap,SpriteMap)
loadUnite renderer unite tmap smap = do 
  tmap' <- TM.loadTexture renderer (getFilePathForUnite (E.typeu unite)) (getTextureIdForUnite (E.typeu unite)) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (getTextureIdForUnite (E.typeu unite)) (S.mkArea 0 0 32 32)
  let smap'=SM.addSprite (getSpriteIdForUnite(E.typeu unite)) sprite smap

  return (tmap',smap')

afficherBatiments::Environement -> TextureMap -> SpriteMap-> Renderer-> IO(TextureMap,SpriteMap)
afficherBatiments gameState@(E.Environement joueurs carte unis bats) tmap smap renderer=
  foldl (\(tmap', smap') (_, bat) ->
            let (tmap'', smap'') = loadBatiment renderer bat tmap' smap'
            in (tmap'', smap'')
        ) (tmap, smap) bats

afficherUnite::Environement -> TextureMap -> SpriteMap-> Renderer-> IO(TextureMap,SpriteMap)
afficherUnite gameState@(E.Environement joueurs carte unis bats) tmap smap renderer=
  foldl (\(tmap', smap') (_, bat) ->
            let (tmap'', smap'') = loadBatiment renderer bat tmap' smap'
            in (tmap'', smap'')
        ) (tmap, smap) unis


main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap
  -- initialisation de l'état du jeu
  let gameState = M.initGameState 2
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  (tmap'',smap'')<- afficherBatiments gameState tmap' smap' renderer
  -- (tmap''',smap''')<- afficherUnite gameState tmap'' smap'' renderer
  gameLoop 60 renderer tmap' smap' kbd gameState

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> IO Environement -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState = --do
  --startTime <- time
  --events <- pollEvents
  --let (kbd', mouse) = K.handleEvents events kbd
  --clear renderer
  --- display background
  --S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  ---
  --present renderer
  --endTime <- time
  --let refreshTime = endTime - startTime
  --let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  --threadDelay $ delayTime * 1000 -- microseconds
  --endTime <- time
  --let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  
  --- update du game state


  --unless (K.keypressed KeycodeEscape kbd') 
    --(gameLoop frameRate renderer tmap smap kbd' gameState
      --)
  
-- (gameLoop frameRate renderer tmap smap kbd' gameState')
