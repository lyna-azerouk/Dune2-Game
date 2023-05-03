module Environement where
import qualified Data.Map.Strict as M
import Data.Ord
import Data.List
import Carte
import Control.Monad (when)
import Control.Monad (foldM)
import System.Random

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

newtype BatId = BatId Int deriving (Eq, Show)

instance Ord BatId where
  (BatId id1) < (BatId id2) = id1 < id2
  (BatId id1) <= (BatId id2) = id1 <= id2
  (BatId id1) > (BatId id2) = id1 > id2
  (BatId id1) >= (BatId id2) = id1 >= id2

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

bid:: Batiment -> BatId
bid batiment@(Batiment _ _ _ id _ _)= id

btype:: Batiment -> String
btype batiment@(Batiment _ _ typeb _ _ _)= case typeb of 
    QG r -> "qg"
    Raffinerie r ->"raffinerie"
    Usine r1 u r2 ->"usine"
    Centrale r -> "centrale"
    _-> "error pas de type bâtiment"
    
newtype UniteId = UniteId Int 
data TypeUnite = Collecteur Int
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
    Collecteur _ -> "collecteur"
    Combattant -> "combattant"

upid :: Unite -> UniteId
upid unite@(Unite _ _ _ idu _ _) = idu

upv :: Unite -> Int
upv unite@(Unite _ _ _ _ pvu _) = pvu

uordres :: Unite -> M.Map Int TypeOrdres
uordres unite@(Unite _ _ _ _ _ ordres) = ordres

ressouceCollecteur :: Unite -> Int
ressouceCollecteur unite@(Unite _ _ typeu _ _ _) =
  case typeu of
    Collecteur n -> n
    _ -> 0

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
    caseBatimentCoord (Batiment c _ _ _ _ _) coordb = c == coordb

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
    all (\(_, bat@( Batiment _ propriob _ _ _ _)) ->case chercheJoueur joueurs propriob of
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

randomCoord :: Int -> Int -> [Coord] -> IO Coord
randomCoord maxX maxY coords = do
  x <- randomRIO (0, maxX)
  y <- randomRIO (0, maxY)
  let coord = Coord x y
  if coord `elem` coords then randomCoord maxX maxY coords else return coord

whileLoop :: Carte -> [Coord] -> IO Coord
whileLoop carte coords = do
  coord <- randomCoord (cartel carte) (carteh carte) coords
  let recup = getCase coord carte
  case recup of
    Just Herbe -> return coord
    _ -> whileLoop carte (coord : coords)

creerListeBatConst :: [Batiment] -> M.Map BatId Batiment -> M.Map BatId Batiment
creerListeBatConst bats res = foldl (\acc bat@(Batiment _ _ _ idb _ _) -> M.insert idb bat acc) res bats


-- constructeur intelligent Environnement 
smartConst_env :: Carte -> [Joueur] -> IO Environement
smartConst_env carte listejoueurs = do
  let coords = [] 
  (batiments, _, _) <- foldM (\(bats, ns, cs) j -> do
                          coord <- whileLoop carte cs
                          let bat = Batiment { coordb = coord 
                                             , propriob= jid j
                                             , typeb = QG 15
                                             , idb = BatId ns
                                             , pvb = 30
                                             , prixb = 0 }  
                          return (bat:bats, ns+1, coord:cs)) ([], 1, coords) listejoueurs
  return $ Environement { joueurs = listejoueurs
                        , ecarte = carte
                        , unites = M.empty
                        , batiments = creerListeBatConst batiments M.empty}

actionRaffinerie :: Unite -> Batiment -> (Unite, Int)
actionRaffinerie unite@(Unite _ _ (Collecteur n) _ _ _) _ =
  let nbrRessources = ressouceCollecteur unite
  in (unite { typeu = Collecteur 0 }, nbrRessources)

propr_pre_actionRaffinerie :: Unite -> Bool
propr_pre_actionRaffinerie (Unite _ _ (Collecteur n) _ _ _) = n > 0
propr_pre_actionRaffinerie _ = False

propr_post_actionRaffinerie :: Unite -> Bool
propr_post_actionRaffinerie (Unite _ _ (Collecteur n) _ _ _) = n == 0
propr_post_actionRaffinerie _ = False

invariant_actionRaffinerie :: Unite -> Bool
invariant_actionRaffinerie (Unite _ _ Combattant _ _ _) = False
invariant_actionRaffinerie (Unite _ _ (Collecteur _) _ _ _) = True
invariant_actionRaffinerie _ = False
