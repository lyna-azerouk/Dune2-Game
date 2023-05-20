{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import qualified SDL.Primitive as SDLp
import Linear.V4 (V4(..))
import Linear.V2 (V2(..))
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Foreign.C.Types (CInt)
import Data.Word (Word8)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')
import SDL (Renderer, V2(..), Rectangle(..), Texture, createTextureFromSurface, copy, destroyTexture, freeSurface)
import qualified SDL.Font as Font
import qualified SDL.Primitive as SDLp
import qualified SDL.Video as SDLVideo
import qualified Data.Text as Text
import Foreign.C.Types (CInt)
import Linear (V4(..))
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
import Control.Monad (foldM)
import qualified Data.Map as Map

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Primitive as SDLPrimitives
import qualified Data.Text as Text
import qualified SDL.Video as SDLVideo

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

afficherBatiments :: Environement -> TextureMap -> SpriteMap -> Renderer -> IO (TextureMap, SpriteMap)
afficherBatiments gameState@(E.Environement joueurs carte unis bats) tmap smap renderer =
  let batTuples = fmap (\bat -> (bat, bat)) bats
  in foldM (\(tmap', smap') (_, (batId, bat)) -> do
               (tmap'', smap'') <- loadBatiment renderer bat tmap' smap'
               return (tmap'', smap'')
           ) (tmap, smap) (Map.toList batTuples)


afficherUnite :: Environement -> TextureMap -> SpriteMap -> Renderer -> IO (TextureMap, SpriteMap)
afficherUnite gameState@(E.Environement joueurs carte unis bats) tmap smap renderer =
  let uniTuples = Map.toList unis
  in foldM (\(tmap', smap') (_, uni) -> do
               (tmap'', smap'') <- loadUnite renderer uni tmap' smap'
               return (tmap'', smap'')
           ) (tmap, smap) uniTuples


-- Dimensions de la fenêtre
windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 650

-- Couleur du rectangle
rectangleColor :: V4 Word8
rectangleColor = V4 255 0 0 255 -- Rouge (RVBA)

-- Couleur de la ligne
lineColor :: V4 Word8
lineColor = V4 0 255 0 255 -- Vert (RVBA)

-- Couleur du texte
textColor :: V4 Word8
textColor = V4 0 0 0 255 -- Noir (RVBA)

main :: IO ()
main = do
  Font.initialize -- Initialise SDL-ttf
  window <- SDL.createWindow "Dunes II" SDL.defaultWindow -- Crée une fenêtre
    { SDL.windowInitialSize = V2 (fromIntegral windowWidth) (fromIntegral windowHeight)
    }

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer -- Crée un rendu pour la fenêtre
  font <- Font.load "assets/rambaultxboldregular.ttf" 24-- Charge une police de caractères

  SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255 -- Définit la couleur de fond du rendu (blanc)
  SDL.clear renderer -- Efface le rendu avec la couleur de fond
  let linePosition= V2 0 0
      lineSize = V2 200 650

  -- Dessine la ligne
  SDLp.fillRectangle renderer linePosition lineSize lineColor
  let rectPosition = V2 35 75  -- Position du rectangle (50/2 = 25 pour le centrage)
      rectSize = V2 150 135 -- Taille du rectangle

  -- Dessine le rectangle
  SDLp.fillRectangle renderer rectPosition rectSize rectangleColor

  let rectPosition2= V2 35 150-- Position du rectangle (50/2 = 25 pour le centrage)
      rectSize2 = V2 150 210 -- Taille du rectangle

  -- Dessine le rectangle
  SDLp.fillRectangle renderer rectPosition2 rectSize2 rectangleColor

  let text = "Actions"

    -- Rendu du texte
  surface <- Font.blended font textColor text -- Rendu du texte sur une surface
  texture <- SDL.createTextureFromSurface renderer surface -- Création d'une texture à partir de la surface
  SDL.freeSurface surface -- Libération de la surface

  -- Position du texte
  let textPosition = V2 45 15 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize = V2 100 50 -- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture Nothing (Just (SDL.Rectangle (SDL.P textPosition) textSize))
 

  let text2 = "Déplacer"

    -- Rendu du texte
  surface2 <- Font.blended font textColor text2 -- Rendu du texte sur une surface
  texture2 <- SDL.createTextureFromSurface renderer surface2 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface2 -- Libération de la surface

  -- Position du texte
  let textPosition2 = V2 50 75 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize2 = V2 75 50 -- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture2 Nothing (Just (SDL.Rectangle (SDL.P textPosition2) textSize2))

  let text3 = "Récolter"

    -- Rendu du texte
  surface3 <- Font.blended font textColor text3 -- Rendu du texte sur une surface
  texture3 <- SDL.createTextureFromSurface renderer surface3 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface3 -- Libération de la surface

  -- Position du texte
  let textPosition3 = V2 50 150 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize3 = V2 75 50-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture3 Nothing (Just (SDL.Rectangle (SDL.P textPosition3) textSize3))

  SDL.present renderer -- Affiche le rendu à l'écran

  loop -- Boucle principale

  SDL.destroyRenderer renderer -- Détruit le rendu
  SDL.destroyWindow window -- Ferme la fenêtre
  SDL.quit -- Quitte SDL

-- Boucle principale pour maintenir la fenêtre ouverte
loop :: IO ()
loop = do
  events <- SDL.pollEvents -- Récupère les événements
  let kbd = K.createKeyboard
  let (kbd', mouse) = K.handleEvents events kbd
  let boutonDeplacer= M.GameState 35 75 0
  let boutonRecolter= M.GameState 35 150 0
  when (M.insideGameState mouse boutonDeplacer)
    (putStrLn "Deplacer!")
  
  when (M.insideGameState mouse boutonRecolter)
    (putStrLn "Recolter !")
  let quit = any isQuitEvent events
  if quit
    then return ()
    else loop

-- Vérifie si un événement est un événement de fermeture de la fenêtre
isQuitEvent :: SDL.Event -> Bool
isQuitEvent event = case SDL.eventPayload event of
  SDL.QuitEvent -> True
  _             -> False



{-main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  --(tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  --(tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap
  -- initialisation de l'état du jeu
  let gameState = M.initGameState 2
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  (tmap', smap') <- gameState >>= \gameState' ->
                  afficherBatiments gameState'  TM.createTextureMap SM.createSpriteMap renderer

  -- (tmap''',smap''')<- afficherUnite gameState tmap'' smap'' renderer
  gameLoop 60 renderer tmap' smap' kbd gameState

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> IO Environement -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState = do
  startTime <- time
  events <- pollEvents
  let (kbd', mouse) = K.handleEvents events kbd
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  
  --- update du game state


  unless (K.keypressed KeycodeEscape kbd') 
    (gameLoop frameRate renderer tmap smap kbd' gameState
      )
  
-- (gameLoop frameRate renderer tmap smap kbd' gameState')-}
