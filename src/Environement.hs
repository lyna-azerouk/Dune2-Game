module Environement where
import qualified Data.Map.Strict as M
import Data.Ord
import Data.List
import Carte
import Control.Monad (when)


newtype JoueurId = JoueurId Int deriving (Eq, Show)
data Joueur = Joueur{
    idj::JoueurId,
    name::String,
    credit::Int
} deriving (Eq, Show)

jid::Joueur -> JoueurId
jid joueur@(Joueur id _ _) = id

jname::Joueur -> String
jname joueur@(Joueur _ name _) = name

jcredit::Joueur -> Int
jcredit joueur@(Joueur _ _ credit) = credit

newtype BatId = BatId Int

data TypeBatiment = QG Int -- Int = energie_prod
    | Raffinerie Int -- Int = energie_conso
    | Usine Int Unite Int -- Int = energie_conso , Unite = unite produite et Int = temps de production
    | Centrale Int -- Int = energie_prod


data Batiment = Batiment{
    coordb::Coord,
    propriob::JoueurId,
    typeb:: TypeBatiment,
    idb::BatId,
    pvb::Int, -- pv si 0 => destruction 
    prixb::Int -- coût du bâtiment en crédits
}
bcoord :: Batiment -> Coord
bcoord batiment@(Batiment coordb _ _ _ _ _) = coordb

bproprio :: Batiment -> JoueurId
bproprio batiment@(Batiment _ propriob _ _ _ _) = propriob

bpv:: Batiment -> Int
bpv batiment@(Batiment _ _ _ _ pvb _)= pvb

bprix:: Batiment -> Int
bprix batiment@(Batiment _ _ _ _ _ prixb)= prixb

bid:: Batiment -> Int
bid batiment@(Batiment _ _ _ id _ _)= id

btype:: Batiment -> String
btype batiment@(Batiment _ _ typeb _ _ _)= case typeb of 
    QG r -> "qg"
    Raffinerie r ->"raffinerie"
    Usine r1 u r2 ->"usine"
    Centrale r -> "centrale"
    _-> "error pas de type bâtiment"
    
newtype UniteId = UniteId Int 
data TypeUnite = Collecteur 
    |Combattant 
    deriving (Eq, Show)

data TypeOrdres = Collecter Int
    | Deplacer Coord
    | Patrouiller Coord Coord
    | Attaquer Coord
    deriving (Eq, Show)
data Unite = Unite{
    coordu::Coord,
    propriou::JoueurId,
    typeu:: TypeUnite,
    idu::UniteId,
    pvu::Int, -- pv si 0 => destruction 
    ordres::M.Map Int TypeOrdres
}

uproprio :: Unite -> JoueurId
uproprio unite@(Unite _ propriou _ _ _ _) = propriou

utype :: Unite -> String
utype unite@(Unite _ _ typeu _ _ _) = case typeu of
    Collecteur -> "collecteur"
    Combattant -> "combattant"

upid :: Unite -> UniteId
upid unite@(Unite _ _ _ idu _ _) = idu

upv :: Unite -> Int
upv unite@(Unite _ _ _ _ pvu _) = pvu

uordres :: Unite -> M.Map Int TypeOrdres
uordres unite@(Unite _ _ _ _ _ ordres) = ordres


data Environement= Environement{
    joueurs:: [Joueur],
    ecarte:: Carte,
    unites:: M.Map UniteId Unite,
    batiments:: M.Map BatId Batiment
}

chercheJoueur :: [Joueur] -> JoueurId -> Maybe Joueur
chercheJoueur [] _ = Nothing
chercheJoueur (joueur:listeJoueurs) idJoueur =
  if (jid joueur)== idJoueur
    then Just joueur
    else chercheJoueur listeJoueurs idJoueur

cherche_Case_Batiment :: Coord -> M.Map BatId Batiment -> [Batiment]
cherche_Case_Batiment coord bats =
  map snd $ filter (\(_, bat) -> caseBatimentCoord bat coord) (M.toList bats)
  where
    caseBatimentCoord (Batiment c _ _ _ _) coordb = c == coordb

cherche_Case_Unite :: Coord ->M.Map UniteId Unite -> [Unite]
cherche_Case_Unite coord unis =
  map snd $ filter (\(_, uni) -> caseUniteCoord uni coord) (M.toList unis)
  where
    caseUniteCoord (Unite c _ _ _ _ _) coordu = c == coordu

-- prop_environnement_1 vérifie que chaque bâtiment et unité appartient à un joueur de l'environnement
prop_environnement_1 :: Environement -> Bool
prop_environnement_1 env@(Environement joueurs _ unites batiments) =
    all (\(_, uni@(Unite _ propriou _ _ _ _)) ->case chercheJoueur joueurs propriou of
                                                Just _ -> True
                                                Nothing -> False) (M.toList unites)
    &&
    all (\(_, bat@( Batiment _ propriob _ _ _)) ->case chercheJoueur joueurs propriob of
                                                Just _ -> True
                                                Nothing -> False) (M.toList batiments)

-- prop_environnement_2 vérifie que chaque case Eau est vide 
prop_environnement_2 :: Environement -> Bool
prop_environnement_2 (Environement _ carte@(Carte _ _ contenu) unites batiments) =
    all (\(c, terrain) -> case terrain of
        Eau -> null (cherche_Case_Batiment c batiments) && null (cherche_Case_Unite c unites)
        _ -> True) (M.toList contenu)

-- prop_environnement_3 vérifie que chaque case Herbe contient au max un bâtiment ou une unité
prop_environnement_3:: Environement -> Bool
prop_environnement_3 (Environement _ carte@(Carte _ _ contenu) unites batiments) =
    all (\(c, terrain) -> case terrain of
        Herbe -> case (cherche_Case_Batiment c batiments) of 
                []-> True
                bat:[] -> True
                _ -> False
            && case (cherche_Case_Unite c unites) of
                []-> True
                uni:[]->True
                _->False
        _ -> True) (M.toList contenu)


-- prop_environnement_4 vérifie que chaque case Ressource contient au max une unité
prop_environnement_4 :: Environement -> Bool
prop_environnement_4 (Environement _ carte@(Carte _ _ contenu) unites batiments) =
    all (\(c, terrain) -> case terrain of
        Eau -> null (cherche_Case_Batiment c batiments) 
                &&  case (cherche_Case_Unite c unites) of
                    []-> True
                    uni:[]->True
                    _->False
        _ -> True) (M.toList contenu)




randomCoord :: Int -> Int -> [Coord] -> Coord
randomCoord maxX maxY coords =
  let x = randomRIO (0, maxX)
      y = randomRIO (0, maxY)
      coord = Coord <$> x <*> y
  in if coord `elem` coords then randomCoord maxX maxY coords else coord

whileLoop :: Carte -> [Coord] -> Coord
whileLoop carte coords = do
  let coord = randomCoord (cartel carte) (carteh carte) coords
  let recup = getTerrain coord carte
  case recup of
    Just Herbe -> coord
    _ -> whileLoop carte (coord : coords)

-- constructeur intelligent Environnement 
smartConst_env :: Carte -> [Joueur] -> Environement
smartConst_env carte listejoueurs = 
  let coords = [] in
  let batiments = foldl (\bats j -> 
                          let coord = whileLoop carte coords 
                              bat = Batiment { coorb = coord 
                                             , propriob= j
                                             , typeb = QG 15
                                             , idb = BatId n
                                             , pvb = 30
                                             , prixb = 0 } 
                                newcoords = coord:coords
                                newn=n+1  
                          in bat:bats) [] listejoueurs
  in Environement { joueurs = listejoueurs
                  , ecarte = carte
                  , unites = M.empty
                  , batiments = batiments }
  where n = 1
