{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified SDL
import qualified SDL.Primitive as SDLp
import Linear.V4 (V4(..))
import Linear.V2 (V2(..))
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Foreign.C.Types (CInt)
import Data.Word (Word8,Word32)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified SDL.Image as IMG
import Text.Read (readMaybe)
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
    E.Raffinerie _ _-> (TextureId "raffinerie")
    E.Usine _ _ _ _ -> (TextureId "usine")
    E.Centrale _ -> (TextureId "centrale")


getFilePathForBatiment::E.TypeBatiment -> FilePath
getFilePathForBatiment batid = 
  case batid of 
    E.QG _ -> "assets/virus.bmp"
    E.Raffinerie _ _-> "assets/virus.bmp"
    E.Usine _ _ _ _ -> "assets/virus.bmp"
    E.Centrale _ -> "assets/virus.bmp"


getSpriteIdForBatiment::E.TypeBatiment -> SpriteId 
getSpriteIdForBatiment batid = 
  case batid of 
    E.QG _ -> (SpriteId "qg")
    E.Raffinerie _ _ -> (SpriteId "raffinerie")
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
  font <- Font.load "assets/rambaultxboldregular.ttf" 50-- Charge une police de caractères

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

  let rectPosition3= V2 35 150-- Position du rectangle (50/2 = 25 pour le centrage)
      rectSize2 = V2 150 210 -- Taille du rectangle

  -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 225)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect

   -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 225)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect

   -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 300)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect
   -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 375)) (V2 115 50))
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
 

  let text2 = "Récolter"

    -- Rendu du texte
  surface2 <- Font.blended font textColor text2 -- Rendu du texte sur une surface
  texture2 <- SDL.createTextureFromSurface renderer surface2 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface2 -- Libération de la surface

  -- Position du texte
  let textPosition2 = V2 50 75 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize2 = V2 75 50 -- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture2 Nothing (Just (SDL.Rectangle (SDL.P textPosition2) textSize2))

  let text3 = "Créer Bâtiment"

    -- Rendu du texte
  surface3 <- Font.blended font textColor text3 -- Rendu du texte sur une surface
  texture3 <- SDL.createTextureFromSurface renderer surface3 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface3 -- Libération de la surface

  -- Position du texte
  let textPosition3 = V2 50 150 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize3 = V2 75 50-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture3 Nothing (Just (SDL.Rectangle (SDL.P textPosition3) textSize3))

  let text4 = "Créer Unite"

    -- Rendu du texte
  surface4 <- Font.blended font textColor text4 -- Rendu du texte sur une surface
  texture4 <- SDL.createTextureFromSurface renderer surface4 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface4 -- Libération de la surface

  -- Position du texte
  let textPosition4 = V2 50 225 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize4 = V2 75 50-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture4 Nothing (Just (SDL.Rectangle (SDL.P textPosition4) textSize4))

  let text4 = "Action Unite"

    -- Rendu du texte
  surface4 <- Font.blended font textColor text4 -- Rendu du texte sur une surface
  texture4 <- SDL.createTextureFromSurface renderer surface4 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface4 -- Libération de la surface

  -- Position du texte
  let textPosition4 = V2 50 300 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize4 = V2 75 50-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture4 Nothing (Just (SDL.Rectangle (SDL.P textPosition4) textSize4))

  let text4 = "Donner Ordre Unite"

    -- Rendu du texte
  surface4 <- Font.blended font textColor text4 -- Rendu du texte sur une surface
  texture4 <- SDL.createTextureFromSurface renderer surface4 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface4 -- Libération de la surface

  -- Position du texte
  let textPosition4 = V2 50 375 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize4 = V2 75 50-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture4 Nothing (Just (SDL.Rectangle (SDL.P textPosition4) textSize4))

  -- Dessin d'une ligne horizontale
  let lineWidth = 2 -- Longueur de la ligne
      lineHeight = 800-- Épaisseur de la ligne
      startOffset = 200 -- Décalage de départ des lignes verticales
  
  SDL.rendererDrawColor renderer SDL.$= textColor -- Couleur de la ligne
  texteCarte <- (readFile $ "assets/map.txt")
  let (C.Carte vx vy contenuCarte) =  C.createCarte texteCarte
  let gameState = M.initGameState 1 (C.Carte vx vy contenuCarte)

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
            E.Raffinerie _ _-> "raffinerie"
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

  SDL.present renderer
  clock <- SDL.ticks
  loop renderer clock gS 2 0 -- Boucle principale
   -- Affiche le rendu à l'écran
  SDL.destroyRenderer renderer -- Détruit le rendu
  SDL.destroyWindow window -- Ferme la fenêtre
  SDL.quit -- Quitte SDL


loop :: Renderer -> Word32 -> Environement -> Int -> Int->IO ()
loop renderer prevTime envi@(E.Environement joueurs ecarte unites bats enn) idglobal temp= do
  let (C.Carte cx cy contenuCarte)=ecarte
  currentTime <- SDL.ticks
  putStrLn (show temp)

  -- Calcul du temps écoulé depuis la dernière mise à jour
  let deltaTime = fromIntegral (currentTime - prevTime) / 1000.0

  events <- SDL.pollEvents -- Récupère les événements
  let kbd = K.createKeyboard
  let (kbd', mouse) = K.handleEvents events kbd
  -- Draw a rectangle occupying the entire window
  font <- Font.load "assets/rambaultxboldregular.ttf" 50-- Charge une police de caractères
  let imageWidth = 45
      imageHeight = 45
      imageWidthmap = 55
      imageHeightmap = 55
      srcRect = Nothing
      linePosition= V2 0 0
      lineSize = V2 200 650

      -- Dessin d'une ligne horizontale
  let lineWidth = 2 -- Longueur de la ligne
      lineHeight = 800-- Épaisseur de la ligne
      startOffset = 200 -- Décalage de départ des lignes verticales
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

  -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 225)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect

   -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 225)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect

   -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 300)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect
   -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 375)) (V2 115 50))
  SDL.copy renderer imageTexture srcRect dstRect

   -- Dessine le rectangle
  imageSurface <- IMG.load "assets/button.png"
  imageTexture <- SDL.createTextureFromSurface renderer imageSurface
  SDL.freeSurface imageSurface
  let dstRect = Just (Rectangle (P (V2 35 450)) (V2 115 50))
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
 

  let text2 = "Récolter"

    -- Rendu du texte
  surface2 <- Font.blended font textColor text2 -- Rendu du texte sur une surface
  texture2 <- SDL.createTextureFromSurface renderer surface2 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface2 -- Libération de la surface

  -- Position du texte
  let textPosition2 = V2 50 90 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize2 = V2 100 25 -- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture2 Nothing (Just (SDL.Rectangle (SDL.P textPosition2) textSize2))

  let text3 = "Créer Batiment"

    -- Rendu du texte
  surface3 <- Font.blended font textColor text3 -- Rendu du texte sur une surface
  texture3 <- SDL.createTextureFromSurface renderer surface3 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface3 -- Libération de la surface

  -- Position du texte
  let textPosition3 = V2 50 160 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize3 = V2 100 25-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture3 Nothing (Just (SDL.Rectangle (SDL.P textPosition3) textSize3))
  
  let text4 = "Créer Unite"

    -- Rendu du texte
  surface4 <- Font.blended font textColor text4 -- Rendu du texte sur une surface
  texture4 <- SDL.createTextureFromSurface renderer surface4 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface4 -- Libération de la surface

  -- Position du texte
  let textPosition4 = V2 50 235 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize4 = V2 100 25-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture4 Nothing (Just (SDL.Rectangle (SDL.P textPosition4) textSize4))
  let text4 = "Action Unite"

    -- Rendu du texte
  surface4 <- Font.blended font textColor text4 -- Rendu du texte sur une surface
  texture4 <- SDL.createTextureFromSurface renderer surface4 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface4 -- Libération de la surface

  -- Position du texte
  let textPosition4 = V2 50 310 -- Position du rectangle (50/2 = 25 pour le centrage)
      textSize4 = V2 100 25-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture4 Nothing (Just (SDL.Rectangle (SDL.P textPosition4) textSize4))

  let text4 = "Donner Ordre Unite"

    -- Rendu du texte
  surface4 <- Font.blended font textColor text4 -- Rendu du texte sur une surface
  texture4 <- SDL.createTextureFromSurface renderer surface4 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface4 -- Libération de la surface

  -- Position du texte
  let textPosition4 = V2 50 385
      textSize4 = V2 100 25-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture4 Nothing (Just (SDL.Rectangle (SDL.P textPosition4) textSize4))

  let text4 = "Recup Unite Usine"

    -- Rendu du texte
  surface4 <- Font.blended font textColor text4 -- Rendu du texte sur une surface
  texture4 <- SDL.createTextureFromSurface renderer surface4 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface4 -- Libération de la surface

  -- Position du texte
  let textPosition4 = V2 50 460
      textSize4 = V2 100 25-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture4 Nothing (Just (SDL.Rectangle (SDL.P textPosition4) textSize4))
  
  let cr = case E.takeFirstPlayer joueurs of
            Just j@(E.Joueur idj name credit _) -> credit 
            Nothing -> 0
  
  let text5 = "Crédit = " ++ show cr
  surface5 <- Font.blended font textColor (Text.pack text5) -- Rendu du texte sur une surface
  texture5 <- SDL.createTextureFromSurface renderer surface5 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface5 -- Libération de la surface

  -- Position du texte
  let textPosition5 = V2 50 500
      textSize5 = V2 100 65-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture5 Nothing (Just (SDL.Rectangle (SDL.P textPosition5) textSize5))

  let energie = case E.takeFirstPlayer joueurs of
            Just j@(E.Joueur idj name credit ener) -> ener
            Nothing -> 0
  
  let text6 = "Energie = " ++ show energie
  surface6 <- Font.blended font textColor (Text.pack text6) -- Rendu du texte sur une surface
  texture6 <- SDL.createTextureFromSurface renderer surface6 -- Création d'une texture à partir de la surface
  SDL.freeSurface surface6 -- Libération de la surface

  -- Position du texte
  let textPosition6 = V2 50 570
      textSize6 = V2 100 65-- Taille du rectangle

  -- Affichage du texte
  SDL.copy renderer texture6 Nothing (Just (SDL.Rectangle (SDL.P textPosition6) textSize6))

  SDL.rendererDrawColor renderer SDL.$= textColor 
  texteCarte <- (readFile $ "assets/map.txt")

  Map.traverseWithKey (\(C.Coord x y) terrain->
    let fichier = terrainToString terrain
        imagePath = "assets/" ++ fichier ++ ".png"  
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


  Map.traverseWithKey (\iduni uni@(E.Unite c@(C.Coord x y) _ t _ _ _) ->
    let fichier = 
          case t of 
            E.Combattant -> "unite_soldat"
            E.Collecteur _ _ -> "unite_collecteur2"
        imagePath = "assets/" ++ fichier ++ ".png" 
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
            E.Raffinerie _ _-> "raffinerie"
            E.Usine _ _ _ _ -> "usine"
            E.Centrale _ -> "centrale"
        imagePath = "assets/" ++ fichier ++ ".bmp"  
    in do
      imageSurface <- IMG.load imagePath
      imageTexture <- SDL.createTextureFromSurface renderer imageSurface
      SDL.freeSurface imageSurface
      let dstRect = Just (Rectangle (P (V2 (fromIntegral (203 + x * 50)) (fromIntegral (4 + y * 50)))) (V2 (fromIntegral imageWidth) (fromIntegral imageHeight)))
      SDL.copy renderer imageTexture srcRect dstRect
      ) bats

  mapM_ (\(C.Coord x y) -> do
    let imagePath = "assets/ennemis.png"  
    imageSurface <- IMG.load imagePath
    imageTexture <- SDL.createTextureFromSurface renderer imageSurface
    SDL.freeSurface imageSurface
    let dstRect = Just (Rectangle (P (V2 (fromIntegral (203 + x * 50)) (fromIntegral (4 + y * 50)))) (V2 (fromIntegral imageWidth) (fromIntegral imageHeight)))
    SDL.copy renderer imageTexture srcRect dstRect
    ) enn

  SDL.present renderer -- Affiche le rendu à l'écran

  let boutonRecolter= M.GameState 35 75 0
  let boutonCreerBat= M.GameState 35 150 0
  let boutonCreerUni= M.GameState 35 225 0
  let boutonActionUnite= M.GameState 35 300 0
  let boutonDonnerOrdre= M.GameState 35 375 0
  let boutonUsine= M.GameState 35 450 0

  when (M.insideGameState mouse boutonRecolter) $ do
    let idj = case E.takeFirstPlayer joueurs of
            Just j@(E.Joueur idj name credit _) -> idj
            Nothing -> error ("pas de joueurs")
        newenvi=E.modifier_credit idj envi
        finalenvi = E.mouvemenEnnemis newenvi
        finalenvi2 = E.miseAjourEnv finalenvi temp
    putStrLn "Récolté";
    let quit = any isQuitEvent events
    loop renderer currentTime finalenvi2 idglobal (temp+1)
  
  when (M.insideGameState mouse boutonCreerBat) $ do
    let j = case E.takeFirstPlayer joueurs of 
            Just jou -> jou
            Nothing -> error ("pas de joueurs")
    putStrLn "nommer le batiment : 1 - raffinerie, 2 - usine ou 3 - centrale"
    type_num<- getLine
    let typeBat = case readMaybe type_num :: Maybe Int of
                    Just val -> case val of 
                                1 -> "raffinerie"
                                2 -> "usine"
                                3 -> "centrale"
                                _-> error "Num type de bâtiment invalide"
                    Nothing -> error "Type de bâtiment invalide"
        prixbat = case E.getPrixBat typeBat of
                    Just pb -> pb
                    Nothing -> error ("pas de prix")
        pvbat = 20
    putStrLn "Donner les coordonnées sur l'axe x"
    str_cx <- getLine
    putStrLn "Donner les coordonnées sur l'axe y"
    str_cy <- getLine 
    let cx = case readMaybe str_cx :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée x invalide")
        cy = case readMaybe str_cy :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée y invalide")
        c = C.Coord cx cy 
    let (val, newenvi) = E.creerBatiment j typeBat envi c idglobal prixbat pvbat
    --print newenvi
    let text = case val of
                True -> "Bâtiment crée!"
                False -> "Bâtiment NON créee!"
    putStrLn text
    let finalenvi = E.mouvemenEnnemis newenvi
        finalenvi2 = E.miseAjourEnv finalenvi temp
    let quit = any isQuitEvent events
    loop renderer currentTime finalenvi2 (idglobal+1) (temp+1)


  when (M.insideGameState mouse boutonCreerUni) $ do
    let j = case E.takeFirstPlayer joueurs of 
            Just jou -> jou
            Nothing -> error ("pas de joueurs")
    putStrLn "Donner les coordonnées de l'usine sur l'axe x"
    str_cx <- getLine
    putStrLn "Donner les coordonnées de l'usine sur l'axe y"
    str_cy <- getLine 
    let cx = case readMaybe str_cx :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée x invalide")
        cy = case readMaybe str_cy :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée y invalide")
        c = C.Coord cx cy 
    putStrLn "Où voulez vous mettre l'unité sur l'axe x"
    str_cx2 <- getLine
    putStrLn "Où voulez vous mettre l'unité sur l'axe y"
    str_cy2 <- getLine 
    let cx2 = case readMaybe str_cx2 :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée x invalide")
        cy2 = case readMaybe str_cy2 :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée y invalide")
        c2 = C.Coord cx2 cy2 
    let (val, newenvi) = E.actionFinUsine envi j c c2 temp idglobal
    let text = case val of
                True -> "Unité créee!"
                False -> "Unité NON créee!"
    --print newenvi
    let finalenvi = E.mouvemenEnnemis newenvi
        finalenvi2 = E.miseAjourEnv finalenvi temp
    putStrLn text
    let quit = any isQuitEvent events
    loop renderer currentTime finalenvi2 (idglobal +1) (temp+1)

  when (M.insideGameState mouse boutonActionUnite) $ do
    putStrLn "Donner les coordonnées de l'unité sur l'axe x"
    str_cx <- getLine
    putStrLn "Donner les coordonnées de l'unité sur l'axe y"
    str_cy <- getLine 
    let cx = case readMaybe str_cx :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée x invalide")
        cy = case readMaybe str_cy :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée y invalide")
        c = C.Coord cx cy 
    let listeuni = E.cherche_Case_Unite c unites in 
      case listeuni of
        []-> error ("pas d'unité à ces coordonnées !")
        u1:[]-> do
                let newenvi = E.etape envi u1
                    finalenvi = E.mouvemenEnnemis newenvi
                    finalenvi2 = E.miseAjourEnv finalenvi temp
                print finalenvi2
                putStrLn "Action Unite!"
                let quit = any isQuitEvent events
                loop renderer currentTime finalenvi2 idglobal (temp+1)
        _ ->error ("trop d'unité à ces coordonnées !")

  when (M.insideGameState mouse boutonDonnerOrdre) $ do
    putStrLn "nommer l'ordre: \n1 - Collecter\n2 - Deplacer\n3 - Patrouiller\n4 - Attaquer\n5 - PoserRaffinerie\n6 - Pause\n7 - Recherche\n"
    type_num<- getLine
    let ordre = case readMaybe type_num :: Maybe Int of
                    Just val -> case val of 
                                1 -> return E.Collecter 
                                2 -> do 
                                    putStrLn "Où aller sur l'axe x"
                                    str_cx <- getLine
                                    putStrLn "Où aller sur l'axe y"
                                    str_cy <- getLine 
                                    let cx = case readMaybe str_cx :: Maybe Int of
                                              Just val -> val
                                              Nothing -> error ("Coordonnée x invalide")
                                        cy = case readMaybe str_cy :: Maybe Int of
                                              Just val -> val
                                              Nothing -> error ("Coordonnée y invalide")
                                        c = C.Coord cx cy in return (E.Deplacer c E.Pause)
                                3 -> do 
                                    putStrLn "patrouille premier coordonnée sur l'axe x"
                                    str_cx <- getLine
                                    putStrLn "patrouille premier coordonnée sur l'axe y"
                                    str_cy <- getLine 
                                    let cx = case readMaybe str_cx :: Maybe Int of
                                              Just val -> val
                                              Nothing -> error ("Coordonnée x invalide")
                                        cy = case readMaybe str_cy :: Maybe Int of
                                              Just val -> val
                                              Nothing -> error ("Coordonnée y invalide")
                                        c = C.Coord cx cy 
                                        
                                    putStrLn "patrouille deuxième coordonnée sur l'axe x"
                                    str_cx2 <- getLine
                                    putStrLn "patrouille deuxième coordonnée sur l'axe y"
                                    str_cy2 <- getLine 
                                    let cx2 = case readMaybe str_cx2 :: Maybe Int of
                                              Just val -> val
                                              Nothing -> error ("Coordonnée x invalide")
                                        cy2 = case readMaybe str_cy2 :: Maybe Int of
                                              Just val -> val
                                              Nothing -> error ("Coordonnée y invalide")
                                        c2 = C.Coord cx2 cy2 in return (E.Patrouiller c c2)
                                4 -> do 
                                    putStrLn "Où attaquer sur l'axe x"
                                    str_cx <- getLine
                                    putStrLn "Où attaquer sur l'axe y"
                                    str_cy <- getLine 
                                    let cx = case readMaybe str_cx :: Maybe Int of
                                              Just val -> val
                                              Nothing -> error ("Coordonnée x invalide")
                                        cy = case readMaybe str_cy :: Maybe Int of
                                              Just val -> val
                                              Nothing -> error ("Coordonnée y invalide")
                                        c = C.Coord cx cy in return (E.Attaquer c E.Pause)
                                5 -> return E.PoserRaffinerie
                                6 -> return E.Pause
                                7 -> return E.Recherche
                                _-> error "Num typeordre d'unité' invalide"
                    Nothing -> error "Type d'ordre invalide"
    
    putStrLn "Donner les coordonnées de l'unité sur l'axe x"
    str_cxuni <- getLine
    putStrLn "Donner les coordonnées de l'unité sur l'axe y"
    str_cyuni <- getLine 
    let cxuni = case readMaybe str_cxuni :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée x invalide")
        cyuni = case readMaybe str_cyuni :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée y invalide")
        cuni = C.Coord cxuni cyuni
    newordre <- ordre
    let newenvi = E.donnerOrdre envi cuni newordre 
        finalenvi = E.mouvemenEnnemis newenvi
        finalenvi2 = E.miseAjourEnv finalenvi temp
    --print newenvi
    putStrLn "Ordre unité donné!"
    let quit = any isQuitEvent events
    loop renderer currentTime finalenvi2 idglobal (temp +1)


  when (M.insideGameState mouse boutonUsine) $ do
    putStrLn "Donner les coordonnées de l'usine sur l'axe x"
    str_cx <- getLine
    putStrLn "Donner les coordonnées de l'usine sur l'axe y"
    str_cy <- getLine 
    let cx = case readMaybe str_cx :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée x invalide")
        cy = case readMaybe str_cy :: Maybe Int of
              Just val -> val
              Nothing -> error ("Coordonnée y invalide")
        c = C.Coord cx cy 
    putStrLn "nommer l'unité à créer dans l'usine: 1 - collecteur, 2 - combattant"
    type_num<- getLine
    let typeuni= case readMaybe type_num :: Maybe Int of
                    Just val -> case val of 
                                1 -> "collecteur"
                                2 -> "combattant"
                                _-> error "Num type d'unité' invalide"
                    Nothing -> error "Type d'unité invalide"

    let newenvi = E.actionUsine envi c temp typeuni
        finalenvi = E.mouvemenEnnemis newenvi
        finalenvi2 = E.miseAjourEnv finalenvi temp
    putStrLn "usine en cours"
    let quit = any isQuitEvent events
    loop renderer currentTime finalenvi2 idglobal (temp+1)


  let finalenvi = E.mouvemenEnnemis envi
      finalenvi2 = E.miseAjourEnv finalenvi temp
  let quit = any isQuitEvent events
  unless quit $ loop renderer currentTime finalenvi2 idglobal (temp+1)

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _ payload) =
  case payload of
    SDL.QuitEvent -> True
    _ -> False

