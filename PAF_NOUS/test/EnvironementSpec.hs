module EnvironementSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map.Strict as M

import Carte
import Environement 

getFirst :: (a, b) -> a
getFirst (x, _) = x

getSec :: (a, b) -> b
getSec (_, x) = x

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
    


---verifie que chaque ebatiment corespond a un joueur
enviTest = do
    describe "Tests environnement" $ do
        it "Environnement initiale" $ do
            prop_environnement_1 env1 `shouldBe` True
        it "check case eau" $ do
            prop_environnement_2 env1 `shouldBe` True
        it "check case Herbe" $ do
            prop_environnement_3 env1 `shouldBe` True
        it "check case Ressource" $ do
            prop_environnement_4 env1 `shouldBe` True
        it "check action rafinerie pre " $ do
            prop_pre_actionRaffinerie unite1 `shouldBe` True
        
        it "check action rafinerie post " $ do
            prop_post_actionRaffinerie  (getFirst (actionRaffinerie unite1 batiment1)) `shouldBe`  True

        it "check action rafinerie invariant " $ do
            invariant_actionRaffinerie unite1 `shouldBe` True

        it "check Batiment precondition 1  " $ do
            prop_pre_creerBatiment1 env1 coord1 `shouldBe` True
        
        it "check Batiment precondition 2  " $ do
            prop_pre_creerBatiment2 env1 (Coord 2 4) `shouldBe` True
          
        it "check Creation batiment  post " $ do
            prop_post_creerBatiment1  (getSec (creerBatiment  joueur1 "raffinerie"   env1 coord2  10 10 10 ))  coord2 "raffinerie" `shouldBe` True   

        it "check Fermiture batiment precondition  " $ do
              prop_pre_actionFermetureBatiment env1 joueur1  `shouldBe` True

        it "check Fermiture batiment postcondition  " $ do
              prop_pre_actionFermetureBatiment  ( actionFermetureBatiment env1 joueur1 ) joueur1 `shouldBe` True

        it "check Action usine Precondition 1" $ do 
          prop_pre_actionUsine1  batiment2 joueur1 `shouldBe` True
        
        it "check Action usine Precondition 2" $ do 
          prop_pre_actionUsine2  batiment2  `shouldBe` True

        it "check Action usine postcondition " $ do 
          prop_post_actionUsine   ( actionUsine batiment2  joueur1 unite1) `shouldBe` True

      
engineSpec = do
  enviTest