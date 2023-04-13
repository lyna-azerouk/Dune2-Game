import qualified Data.Map.Strict as M


data Coord = C {cx :: Int ,
cy :: Int}
deriving (Show , Eq)


data Terrain = Herbe
| Ressource Int
| Eau


newtype Carte = Carte {carte:: M.Map Coord Terrain}

--modifier une case donnee dans une carte
editCase :: Coord -> Terrain -> Carte -> Carte
editCase coord t carte = Carte (carteh carte) (cartel carte) (M.insert coord t (carte_contenu carte))



---------------------INVARIANTS----------------------

prop_positiveCoord_inv :: Coord -> Bool
prop_positiveCoord_inv co = ((cx co) >= 0) && ((cy co) >= 0)

-- Vérifie qu'une coordonnée donee est entre les bornes données par la largeur et hauteur
coordInBounds :: Coord -> Int -> Int -> Bool
coordInBounds co larg haut = (prop_positiveCoord_inv co)
                          && ( ( (cx co) < larg ) 
                          && ( (cy co) < haut ) )



-- Vérifie que toutes les coordonnées des cases sont entre les bornes données par la largeur et hauteur de la carte
--prop_allCoordsInBounds_inv :: Carte -> Bool
--prop_allCoordsInBounds_inv carte = foldl (\boolAcc (co,_) -> boolAcc && coordInBounds co (cartel carte) (carteh carte) ) True (listFromCarte carte)

-------
-- Vérifie qu'une coordonnée donnée correspond bien à une case dans la carte
coordInCarte :: Coord -> Carte -> Bool
coordInCarte coord carte = case (getCase coord carte) of
                    Just _ -> True
                    Nothing -> False