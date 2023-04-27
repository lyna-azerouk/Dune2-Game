module Carte where
import qualified Data.Map.Strict as M
import Data.Ord
import Data.List



---------------------INSTANCES----------------------

data Coord = Coord {cx :: Int ,cy :: Int}deriving (Show , Eq)


data Terrain = Herbe
    | Ressource Int
    | Eau
    deriving (Eq, Show)
--cartel longeur de la carte 
--carteh hauteur de la carte 
data Carte = Carte {cartel :: Int ,carteh :: Int ,carte_contenu:: M.Map Coord Terrain}

---------------------INSTANCES----------------------
-- définition de la maniere d'ordonner des coordonnees
instance Ord Coord where
    compare c1 c2
        | (cy c1) < (cy c2) = LT
        | ((cy c1) == (cy c2)) && ((cx c1) < (cx c2)) = LT
        | ((cy c1) == (cy c2)) && ((cx c1) == (cx c2)) = EQ
        | otherwise = GT

instance Read Carte where
    readsPrec _ x = [((createCarte ({-reverse-} x)), "")]

-- fonction auxiliaire au foldl pour ajouter une case en fonction de son type et avec ses coordonnees x et y dans une carte donnee
createCarteAux :: Carte -> Char -> Carte
createCarteAux c@(Carte {cartel = cl, carteh = ch, carte_contenu = cc}) '\n' = c {cartel = 0, carteh = ch + 1, carte_contenu = cc}
createCarteAux c@(Carte {cartel = cl, carteh = ch, carte_contenu = cc}) caractere = c {cartel = cl + 1, carteh = ch, carte_contenu = M.insert (Coord (cl) (ch - 1)) (caseFromChar caractere) cc }

-- creer une carte à partir d'une chaine de caractere
createCarte :: String -> Carte
createCarte texte = foldl createCarteAux (Carte 0 1 M.empty) texte

--Show une instance pour afficher la carte 
instance Show Carte where
    show = toString

class ToString a where
    toString :: a -> String

toStringCarteAux :: Int -> (Coord, Terrain) -> String
toStringCarteAux cartel (co, t) = if (cx co) == (max 0 (cartel - 1)) then (strFromCase t) ++ "\n" else (strFromCase t)


instance ToString Carte where
    toString c = -- "largeur = " ++ (show (cartel c)) ++
                -- "\nhauteur = " ++ (show (carteh c)) ++ "\n" ++
                foldl (\accstr cur -> accstr ++ (toStringCarteAux (cartel c) cur) ) "" (listFromCarte c)



---------------------UTILITAIRES----------------------
-- Renvoie le type de la case correspondant à un caractère
caseFromChar :: Char -> Terrain
caseFromChar caractere = case caractere of
    'H' -> Herbe
    'E' -> Eau

-- transforme une carte en liste de couples coordonnees / Terrain
listFromCarte :: Carte -> [(Coord,Terrain)]
listFromCarte carte = (sortBy (comparing fst) (M.assocs (carte_contenu carte) ))

-- Renvoie le caractère  correspondant à un type de case donné
strFromCase :: Terrain -> String
strFromCase ca = case ca of
    Herbe -> "H"
    Eau -> "E"

getCoord :: (Coord, Terrain) -> Coord
getCoord (c, _) = c


---------------------OPERATIONS----------------------
-- recuperer une case à des coordonnees donnees
getCase :: Coord -> Carte -> Maybe Terrain
getCase coord carte = M.lookup coord (carte_contenu carte)

-- modifier une case donnee dans une carte
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


-- Vérifie qu'une coordonnée donnée correspond bien à une case dans la carte
coordInCarte :: Coord -> Carte -> Bool
coordInCarte coord carte = case (getCase coord carte) of
                    Just _ -> True
                    Nothing -> False

-- Vérifie que toutes les coordonnées avec comme borne la largeur et hauteur correspondent bien à des cases dans la carte
prop_allCoordInCarte_inv :: Carte -> Bool
prop_allCoordInCarte_inv carte = foldl (\boolAcc (x,y) -> boolAcc && coordInCarte (Coord x y) carte) True ((\ i j -> (i, j)) <$> [0..((cartel carte) - 1)] <*> [0..((carteh carte) - 1)])

prop_Coord_inv :: Coord -> Bool
prop_Coord_inv coord = prop_positiveCoord_inv coord


collecteCase :: Coord -> Int -> Carte -> (Int, Carte)
collecteCase coord@(Coord x y) r carte@(Carte _ _ contenu)
  | M.notMember coord contenu = (0, carte) -- if the Coord is not in the map, return 0 resources and the original map
  | otherwise = case contenu M.! coord of
      Herbe -> (0, carte) -- if the Coord contains Herbe, return 0 resources and the original map
      Eau -> (0, carte) -- if the Coord contains Eau, return 0 resources and the original map
      Ressource n ->
        let v = min n r -- calculate the amount of resources to collect
            nouvelleContenu = if v < n then M.insert coord (Ressource (n-v)) contenu else M.insert coord Herbe contenu -- update the map with the new resource amount or replace the resource with Herbe
        in (v, Carte (cartel carte) (carteh carte) nouvelleContenu) -- return the collected amount and the updated map

--précondition : 
--vérifie que coord est une case dans carte,
--r >= 0
--que le contenue de cette est case correspond bien à un terrain
prop_collecteCase_pre :: Coord -> Int -> Carte -> Bool
prop_collecteCase_pre coord r carte =
  M.member coord (carte_contenu carte) && 
  (r >= 0) &&
  case (carte_contenu carte) M.! coord of
    Ressource n -> True
    Eau -> True
    Herbe -> True
    _ -> False


--postcondition : 
-- La quantité extraite v est un entier positif,
-- La quantité extraite v est inférieure ou égale à r ou à extractible, selon le cas
-- La quantité de ressources dans la nouvelle case correspond à la quantité dans l'ancienne case moins la quantité extraite
-- Toutes les autres cases sont inchangées
prop_collecteCase :: Coord -> Int -> Carte -> (Int, Carte) -> Bool
prop_collecteCase coord r carte (v, nc) =
  let
    old_case = carte_contenu carte M.! coord
    new_case = carte_contenu nc M.! coord
    extractible = case old_case of { Ressource n -> n; _ -> 0 }
  in
    (v >= 0)
    && v <= r || v <= extractible
    && case old_case of { Ressource n -> n - v; _ -> 0 } == case new_case of { Ressource n -> n; _ -> 0 }
    && all (\c -> c == coord || carte_contenu carte M.! c == carte_contenu nc M.! c) (M.keys (carte_contenu carte))
