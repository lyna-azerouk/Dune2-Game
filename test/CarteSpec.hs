module CarteSpec where

import Test.Hspec
import Test.QuickCheck

import Carte

genCarte :: Gen Carte               --exepmle simple de carte 
genCarte = pure (read   "HEEEEH")  --a chaque element en lui associer la fonction read   H:pour herbe et E: pour Eau 

genCoord :: Gen Coord
genCoord = do
  x <- choose (0, 5)
  y <- choose (0, 5)
  pure (Coord x y)



--------------------------
--verifie que toutes les coordonnees de la carte correspondent a une case
prop_carteCoordInBounds_inv :: Property
prop_carteCoordInBounds_inv = forAll genCarte $ prop_allCoordsInBounds_inv


carteAllCoordsInBoundsSpec = do
  describe "Carte ---------> allCoordsInBounds_inv : " $ do
    it "verifie que toutes les coordonnees de la carte correspondent a une case" $
      property prop_carteCoordInBounds_inv




--------------------------------------------
--verifie que toutes les coordonnees entre la hauteur et la largeur donnent vers une case dans la carte
carteAllCaseExistsSpec = do
  describe "Carte ---------> allCoordInCarte_inv : " $ do
    it "verifie que toutes les coordonnees entre la hauteur et la largeur donnent vers une case dans la carte" $
      property prop_AllCoordInCarteCarte_inv

prop_AllCoordInCarteCarte_inv :: Property
prop_AllCoordInCarteCarte_inv = forAll genCarte $ prop_allCoordInCarte_inv

---------- --------------

--verifie que les coordonnées sont valides
carteCoordSpec = do
  describe "Carte ---------> Coord" $ do
    it "verifie que les coordonnées sont valides" $
      property prop_CoordCarte_inv

prop_CoordCarte_inv :: Property
prop_CoordCarte_inv = forAll genCoord $ prop_Coord_inv

----------------------------------------------------


    