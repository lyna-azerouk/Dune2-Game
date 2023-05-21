module EnvironnementQuickcheck where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map.Strict as M
import Carte
import Environement

boolToString :: Bool -> String
boolToString b = if b then "True" else "False"

coord1::Coord 
coord1= Coord 0 0

coord2::Coord 
coord2= Coord 0 1

unite1::Unite
unite1 =(Unite (Coord 0 0) (JoueurId 1) (Collecteur 5 5) (UniteId 10) 10 Pause)

batiment1::Batiment 
batiment1 = (Batiment  (Coord 0 0) (JoueurId 1) ( Raffinerie 5) (BatId 5)  10 10 True)


batiment2::Batiment 
batiment2 = (Batiment  (Coord 0 0) (JoueurId 1) ( Usine 5 unite1 5 "vide") (BatId 5)  10 10 True)

joueur1::Joueur
joueur1= Joueur (JoueurId 1) "lyna" 10

env1::Environement
env1= Environement  [joueur1] ( createCarte "HHRRRHHHEEE")     (M.fromList[ ((UniteId 10) , unite1) ])   (M.fromList[ ((BatId 5) , batiment1) ]) 

instance Arbitrary Environement where
    arbitrary = genEnvironement   

instance Arbitrary Batiment where
    arbitrary = genBatiment   

instance Show Environement where
    show = showEnvi

instance Show Unite where
    show = showUnite 

instance Show Batiment where
    show = showBatiment

showEnvi :: Environement -> String
showEnvi (Environement _ c _ _) = show c 

showUnite:: Unite->String 
showUnite  (Unite c  idj typeu  idu pvu ordre ) =(show c ) ++ (show idj ) ++(show typeu )  ++ (show ordre)  ---j'arrive pas a affiché les int 

showBatiment:: Batiment->String 
showBatiment  (Batiment c  idj typeb  idb pvu _ utilisab ) =(show c ) ++ (show idj )  ++(show idb)  ++ (boolToString utilisab) 

genEnvironement ::Gen Environement 
genEnvironement = return $  env1

genUnite :: Gen Unite 
genUnite= return $ unite1  --on peut utilisé des données aléatoire 

genBatiment:: Gen Batiment
genBatiment= return $ batiment2

prop_environnement_inv ::Property
prop_environnement_inv  = forAll  genEnvironement $ prop_environnement_1 

prop_raffinerie_inv::Property
prop_raffinerie_inv =forAll  genUnite $ invariant_actionRaffinerie

prop_actionUsine_inv::Property
prop_actionUsine_inv =forAll  genBatiment $ invariant_actionUsine


entiteSpec = do
  describe "QuickCheck Environnement  invariant" $ do
    it "Teste l'invariant pour environement1" $ 
      property prop_environnement_inv

    it "Teste l'invariant pour  Action raffinerie" $ 
      property prop_raffinerie_inv
    
    it "Test invariant pour Action usine" $
        property prop_actionUsine_inv

      