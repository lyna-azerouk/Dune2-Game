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
import qualified SDL.Image as IMG

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
import Carte (Carte)
import qualified Carte as C

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


--afficherCase::Carte->

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
afficherBatiments gameState@(E.Environement joueurs carte unis bats _) tmap smap renderer =
  let batTuples = fmap (\bat -> (bat, bat)) bats
  in foldM (\(tmap', smap') (_, (batId, bat)) -> do
               (tmap'', smap'') <- loadBatiment renderer bat tmap' smap'
               return (tmap'', smap'')
           ) (tmap, smap) (Map.toList batTuples)


afficherUnite :: Environement -> TextureMap -> SpriteMap -> Renderer -> IO (TextureMap, SpriteMap)
afficherUnite gameState@(E.Environement joueurs carte unis bats _) tmap smap renderer =
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

-- Couleur bleue
blueColor :: V4 Word8
blueColor = V4 0 0 255 255 -- Bleu (RVBA)

-- Couleur grise
grayColor :: V4 Word8
grayColor = V4 128 128 128 255 -- Gris (RVBA)

gridSize :: Int
gridSize = 50

gridColor :: SDL.V4 Word8
gridColor = V4 0 0 0 255 -- Noir opaque

getColorCase::C.Terrain -> SDL.V4 Word8
getColorCase terrain = 
  case terrain of
    C.Herbe -> lineColor
    C.Eau -> blueColor
    C.Ressource _ -> brownColor

terrainToString::C.Terrain -> String
terrainToString terrain = 
  case terrain of
    C.Herbe -> "Herbe"
    C.Eau -> "Eau"
    C.Ressource _ -> "Ressource"
    
-- Couleur marron
brownColor :: V4 Word8
brownColor = V4 255 255 0 255

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

  let imageX = 100
      imageY = 100
      imageWidth = 45
      imageHeight = 45
      imageWidthmap = 55
      imageHeightmap = 55
      srcRect = Nothing

  
  -- Dessine la ligne
  SDLp.fillRectangle renderer linePosition lineSize grayColor
  --let rectPosition = V2 35 75  -- Position du rectangle (50/2 = 25 pour le centrage)
      --rectSize = V2 150 135 -- Taille du rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 75)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect
  -- Dessine le rectangle
  --SDLp.fillRectangle renderer rectPosition rectSize rectangleColor

  let rectPosition2= V2 35 150-- Position du rectangle (50/2 = 25 pour le centrage)
      rectSize2 = V2 150 210 -- Taille du rectangle

  -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 150)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect
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

  -- Dessin d'une ligne horizontale
  let lineWidth = 2 -- Longueur de la ligne
      lineHeight = 800-- Épaisseur de la ligne
      startOffset = 200 -- Décalage de départ des lignes verticales
  
  SDL.rendererDrawColor renderer SDL.$= textColor -- Couleur de la ligne

   {-- Dessin des lignes verticales
  let verticalLines = map (\x -> fromIntegral (x + startOffset)) [0, gridSize .. windowWidth]
  mapM_ (\x -> SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (V2 x 0)) (V2 lineWidth lineHeight)))) verticalLines

   -- Dessin des lignes horizontales
  let lineWidth2 = fromIntegral (windowWidth - startOffset)
      lineHeight2 = 2
      horizontalLines = map (\y -> fromIntegral y) [0, gridSize .. windowHeight]
  mapM_ (\y -> SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 (fromIntegral startOffset) y)) (V2 lineWidth2 lineHeight2)))) horizontalLines-}
  texteCarte <- (readFile $ "assets/map.txt")
  let (C.Carte vx vy contenuCarte) =  C.createCarte texteCarte
  let gameState = M.initGameState 1 (C.Carte vx vy contenuCarte)
  --Map.traverseWithKey (\(C.Coord x y) terrain -> let color = getColorCase terrain in SDLp.fillRectangle renderer (V2 (fromIntegral 200+((fromIntegral x)*50)) (fromIntegral y*50)) (V2 (fromIntegral 250+((fromIntegral x)*53)) (fromIntegral y*61)+50) color) contenuCarte
  

  Map.traverseWithKey (\(C.Coord x y) terrain->
    let fichier = terrainToString terrain
        imagePath = "assets/" ++ fichier ++ ".png"  -- Chemin vers votre image
    in do
      imageSurface <- IMG.load imagePath
      imageTexture <- SDL.createTextureFromSurface renderer imageSurface
      SDL.freeSurface imageSurface
      let dstRect = Just (Rectangle (P (V2 (fromIntegral (200 + x * 50)) (fromIntegral (y * 50)))) (V2 (fromIntegral imageWidthmap) (fromIntegral imageHeightmap)))
      SDL.copy renderer imageTexture srcRect dstRect
      ) contenuCarte
  
  SDL.rendererDrawColor renderer SDL.$= textColor -- Couleur de la ligne
  -- Dessin des lignes verticales
  let verticalLines = map (\x -> fromIntegral (x + startOffset)) [0, gridSize .. windowWidth]
  mapM_ (\x -> SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (V2 x 0)) (V2 lineWidth lineHeight)))) verticalLines

   -- Dessin des lignes horizontales
  let lineWidth2 = fromIntegral (windowWidth - startOffset)
      lineHeight2 = 2
      horizontalLines = map (\y -> fromIntegral y) [0, gridSize .. windowHeight]
  mapM_ (\y -> SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 (fromIntegral startOffset) y)) (V2 lineWidth2 lineHeight2)))) horizontalLines
  

  gS <- gameState
  let (E.Environement joueurs ecarte unites bats ennemis)= gS
  Map.traverseWithKey (\iduni uni@(E.Unite c@(C.Coord x y) _ t _ _ _) ->
    let fichier = 
          case t of 
            E.Combattant -> "unite_soldat"
            E.Collecteur _ _ -> "unite_collecteur2"
        imagePath = "assets/" ++ fichier ++ ".png"  -- Chemin vers votre image
    in do
      imageSurface <- IMG.load imagePath
      imageTexture <- SDL.createTextureFromSurface renderer imageSurface
      SDL.freeSurface imageSurface
      let dstRect = Just (Rectangle (P (V2 (fromIntegral (203 + x * 50)) (fromIntegral (4 + y * 50)))) (V2 (fromIntegral imageWidth) (fromIntegral imageHeight)))
      SDL.copy renderer imageTexture srcRect dstRect
      ) unites

  Map.traverseWithKey (\idbat bat@(E.Batiment c@(C.Coord x y) _ t _ _ _ _) ->
    let fichier = 
          case t of 
            E.QG _ -> "qg"
            E.Raffinerie _ -> "raffinerie"
            E.Usine _ _ _ _ -> "usine"
            E.Centrale _ -> "centrale"
        imagePath = "assets/" ++ fichier ++ ".bmp"  -- Chemin vers votre image
    in do
      imageSurface <- IMG.load imagePath
      imageTexture <- SDL.createTextureFromSurface renderer imageSurface
      SDL.freeSurface imageSurface
      let dstRect = Just (Rectangle (P (V2 (fromIntegral (203 + x * 50)) (fromIntegral (4 + y * 50)))) (V2 (fromIntegral imageWidth) (fromIntegral imageHeight)))
      SDL.copy renderer imageTexture srcRect dstRect
      ) bats
  
  mapM_ (\(C.Coord x y) -> do
          let imagePath = "assets/ennemis.png"  -- Chemin vers votre image
          imageSurface <- IMG.load imagePath
          imageTexture <- SDL.createTextureFromSurface renderer imageSurface
          SDL.freeSurface imageSurface
          let dstRect = Just (Rectangle (P (V2 (fromIntegral (203 + x * 50)) (fromIntegral (4 + y * 50)))) (V2 (fromIntegral imageWidth) (fromIntegral imageHeight)))
          SDL.copy renderer imageTexture srcRect dstRect
      ) ennemis

  SDL.present renderer -- Affiche le rendu à l'écran

  loop -- Boucle principale

  SDL.destroyRenderer renderer -- Détruit le rendu
  SDL.destroyWindow window -- Ferme la fenêtre
  SDL.quit -- Quitte SDL
  let curent_game = M.initGameState 2


-- Boucle principale pour maintenir la fenêtre ouverte
loop :: IO ()
loop = do
  events <- SDL.pollEvents -- Récupère les événements
  let kbd = K.createKeyboard
  let (kbd', mouse) = K.handleEvents events kbd
  let boutonDeplacer= M.GameState 35 75 0
  let boutonRecolter= M.GameState 35 150 0
  when (M.insideGameState mouse boutonDeplacer) $ do
                            putStrLn "Donner les coordoné  cx"
                            str_cx <-getLine ; 
                            putStrLn "Donner les coordoné  cy";
                            str_cy <-getLine 
                            let cx = read  str_cx ::Int
                            let cy = read  str_cy ::Int
                            let  crd=C.Coord  cx cy 
                            --recupère les unités  qui corespond aux coordones  (faut changer la liste empty)
                            let liste_unit= E.cherche_Case_Unite   crd   Map.empty 
                            --parcours la liste value_unite pour les faire deplacer 
                            curent_game   <-curent_game_
                            new_list_environement<- map   (\unt ->E.actionDeplacerUnite curent_game unt  E.Bas ) 
                            putStrLn "unite moved ";
  
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
