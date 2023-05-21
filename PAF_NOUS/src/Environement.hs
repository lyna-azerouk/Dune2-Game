module Environement where
import qualified Data.Map.Strict as M
import Data.Ord
import Data.List
import Carte
import Control.Monad (when)
import Control.Monad (foldM)
import System.Random
import qualified Data.Set as S
import Control.Monad (replicateM)

newtype JoueurId = JoueurId Int deriving (Eq, Show)
data Joueur = Joueur{
    idj::JoueurId,
    name::String,
    credit::Int,
    energie::Int
} deriving (Eq, Show)

creerJoueur::Int ->[Joueur]
creerJoueur nbr = foldl (\acc n -> (Joueur {
                        idj= JoueurId n,
                        name= "joueur"++ (show n),
                        credit=25, energie=5
                    }):acc) [] [1..nbr]
   
jid::Joueur -> JoueurId
jid joueur@(Joueur id _ _ _) = id

jname::Joueur -> String
jname joueur@(Joueur _ name _ _) = name

jcredit::Joueur -> Int
jcredit joueur@(Joueur _ _ credit _) = credit

jenergie::Joueur -> Int
jenergie joueur@(Joueur _ _ _ energie) = energie

newtype BatId = BatId Int deriving (Eq, Show)

instance Ord BatId where
  (BatId id1) < (BatId id2) = id1 < id2
  (BatId id1) <= (BatId id2) = id1 <= id2
  (BatId id1) > (BatId id2) = id1 > id2
  (BatId id1) >= (BatId id2) = id1 >= id2

data TypeBatiment = QG Int -- Int = energie_prod
    | Raffinerie Int Int -- Int = energie_conso
    | Usine Int String Int String-- Int = energie_conso , Unite = unite produite et Int = temps de production, String = Etat
    | Centrale Int -- Int = energie_prod
    deriving (Show)

data Batiment = Batiment{
    coordb::Coord,
    propriob::JoueurId,
    typeb:: TypeBatiment,
    idb::BatId,
    pvb::Int, -- pv si 0 => destruction 
    prixb::Int, -- coût du bâtiment en crédits
    utilisable::Bool
}deriving (Show)

bcoord :: Batiment -> Coord
bcoord batiment@(Batiment coordb _ _ _ _ _ _) = coordb

bproprio :: Batiment -> JoueurId
bproprio batiment@(Batiment _ propriob _ _ _ _ _) = propriob

bpv:: Batiment -> Int
bpv batiment@(Batiment _ _ _ _ pvb _ _)= pvb

bprix:: Batiment -> Int
bprix batiment@(Batiment _ _ _ _ _ prixb _)= prixb

bid:: Batiment -> BatId
bid batiment@(Batiment _ _ _ id _ _ _)= id

btype:: Batiment -> String
btype batiment@(Batiment _ _ typeb _ _ _ _)= case typeb of 
    QG r -> "qg"
    Raffinerie r _->"raffinerie"
    Usine r1 u r2 etat ->"usine"
    Centrale r -> "centrale"
    _-> "error pas de type bâtiment"

btinitialisation:: String -> Maybe TypeBatiment
btinitialisation typeb= case typeb of
    "raffinerie" -> Just (Raffinerie 10 5)
    "usine" -> Just (Usine 10 "combattant" 0 "vide")
    "centrale" -> Just (Centrale 15)
    _-> Nothing

uniinitialisation:: String -> Maybe TypeUnite
uniinitialisation typeb= case typeb of
    "combattant" -> Just Combattant 
    "collecteur" -> Just (Collecteur 0 4)
    _-> Nothing
    
getPrixBat:: String -> Maybe Int
getPrixBat typeb= case typeb of
    "raffinerie" -> Just 10 
    "usine" -> Just 15
    "centrale" -> Just 15
    _-> Nothing

newtype UniteId = UniteId Int  deriving (Show)
instance Eq UniteId where
    (UniteId a) == (UniteId b) = a == b

instance Ord UniteId where
  (UniteId id1) < (UniteId id2) = id1 < id2
  (UniteId id1) <= (UniteId id2)  = id1 <= id2
  (UniteId id1) > (UniteId id2)  = id1 > id2
  (UniteId id1) >= (UniteId id2)  = id1 >= id2
data TypeUnite = Collecteur Int Int 
    |Combattant 
    deriving (Eq, Show)

data TypeOrdres = Collecter
    | Deplacer Coord TypeOrdres
    | Patrouiller Coord Coord
    | Attaquer Coord TypeOrdres
    | PoserRaffinerie
    | Pause
    | Recherche
    deriving (Eq, Show)
data Unite = Unite{
    coordu::Coord,
    propriou::JoueurId,
    typeu:: TypeUnite,
    idu::UniteId,
    pvu::Int, -- pv si 0 => destruction 
    ordres::TypeOrdres
}deriving (Show)

instance Eq Unite where 
    (Unite c1 p1 t1 i1 pv1 or1) == (Unite c2 p2 t2 i2 pv2 or2)= (c1==c2) && (p1==p2) && (t1==t2) && (i1==i2) && (pv1==pv2) && (or1==or2)


uproprio :: Unite -> JoueurId
uproprio unite@(Unite _ propriou _ _ _ _) = propriou

utype :: Unite -> String
utype unite@(Unite _ _ typeu _ _ _) = case typeu of
    Collecteur _ _-> "collecteur"
    Combattant -> "combattant"

upid :: Unite -> UniteId
upid unite@(Unite _ _ _ idu _ _) = idu

upv :: Unite -> Int
upv unite@(Unite _ _ _ _ pvu _) = pvu

uordres :: Unite -> TypeOrdres
uordres unite@(Unite _ _ _ _ _ ordres) = ordres

ressouceCollecteur :: Unite -> Int
ressouceCollecteur unite@(Unite _ _ typeu _ _ _) =
  case typeu of
    Collecteur n _-> n
    _ -> 0

data Environement= Environement{
    joueurs:: [Joueur],
    ecarte:: Carte,
    unites:: M.Map UniteId Unite,
    batiments:: M.Map BatId Batiment,
    ennemis::[Coord]
} deriving (Show)

takeFirstPlayer :: [Joueur] -> Maybe Joueur
takeFirstPlayer [] = Nothing
takeFirstPlayer (joueur:_) = Just joueur

takeFirstCoord :: [Coord] -> Maybe Coord
takeFirstCoord [] = Nothing
takeFirstCoord (coord:_) = Just coord

takeFirstBat:: [Batiment] -> Maybe Batiment
takeFirstBat [] = Nothing
takeFirstBat (bat:_) = Just bat

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
    caseBatimentCoord (Batiment c _ _ _ _ _ _) coordb = c == coordb

cherche_Case_Unite :: Coord ->M.Map UniteId Unite -> [Unite]
cherche_Case_Unite coord unis =
  map snd $ filter (\(_, uni) -> caseUniteCoord uni coord) (M.toList unis)
  where
    caseUniteCoord (Unite c _ _ _ _ _) coordu = c == coordu

cherche_Case_Ennemis :: Coord -> [Coord]-> [Coord]
cherche_Case_Ennemis coord enns =
  filter (\ c1 -> caseUniteCoord c1 coord) enns
  where
    caseUniteCoord c coordu = c == coordu

-- prop_environnement_1 vérifie que chaque bâtiment et unité appartient à un joueur de l'environnement
prop_environnement_1 :: Environement -> Bool
prop_environnement_1 env@(Environement joueurs _ unites batiments _) =
    all (\(_, uni@(Unite _ propriou _ _ _ _)) ->case chercheJoueur joueurs propriou of
                                                Just _ -> True
                                                Nothing -> False) (M.toList unites)
    &&
    all (\(_, bat@( Batiment _ propriob _ _ _ _ _)) ->case chercheJoueur joueurs propriob of
                                                Just _ -> True
                                                Nothing -> False) (M.toList batiments)

-- prop_environnement_2 vérifie que chaque case Eau est vide 
prop_environnement_2 :: Environement -> Bool
prop_environnement_2 (Environement _ carte@(Carte _ _ contenu) unites batiments _) =
    all (\(c, terrain) -> case terrain of
        Eau -> null (cherche_Case_Batiment c batiments) && null (cherche_Case_Unite c unites)
        _ -> True) (M.toList contenu)

-- prop_environnement_3 vérifie que chaque case Herbe contient au max un bâtiment ou une unité
prop_environnement_3:: Environement -> Bool
prop_environnement_3 (Environement _ carte@(Carte _ _ contenu) unites batiments _) =
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
prop_environnement_4 (Environement _ carte@(Carte _ _ contenu) unites batiments _) =
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

whileLoopEnnemis :: Carte ->[Coord] -> IO Coord
whileLoopEnnemis carte coords = do
  coord <- randomCoord (cartel carte) (carteh carte) coords
  return coord
  

creerListeBatConst :: [Batiment] -> M.Map BatId Batiment -> M.Map BatId Batiment
creerListeBatConst bats res = foldl (\acc bat@(Batiment _ _ _ idb _ _ _) -> M.insert idb bat acc) res bats

creerListeEnnemis :: Carte -> [Coord] -> IO [Coord]
creerListeEnnemis carte coords = replicateM 6 (whileLoop carte coords)

-- constructeur intelligent Environnement 
smartConst_env :: Carte -> [Joueur] -> IO Environement
smartConst_env carte listejoueurs = do
  let coords = [] 
  (batiments, _, coords) <- foldM (\(bats, ns, cs) j -> do
                          coord <- whileLoop carte cs
                          let bat = Batiment { coordb = coord 
                                             , propriob= jid j
                                             , typeb = QG 15
                                             , idb = BatId ns
                                             , pvb = 30
                                             , prixb = 0 
                                             , utilisable = True}  
                          return (bat:bats, ns+1, coord:cs)) ([], 1, coords) listejoueurs
  listeEnnemis <- creerListeEnnemis carte coords
  return $ Environement { joueurs = listejoueurs
                        , ecarte = carte
                        , unites = M.empty
                        , batiments = creerListeBatConst batiments M.empty
                        , ennemis = listeEnnemis
                        }

---------------------
-- PARTIE BATIMENT --
---------------------


actionRaffinerie :: Unite -> Batiment -> (Unite, Batiment)
actionRaffinerie unite@(Unite _ _ (Collecteur n max) _ _ _) bat@(Batiment _ _ t@(Raffinerie ener ressources) _ _ _ True) =
    let nbrRessources = ressouceCollecteur unite in (unite { typeu = Collecteur 0 max}, bat{typeb= Raffinerie ener (ressources + nbrRessources)})
actionRaffinerie unite@(Unite _ _ (Collecteur n max) _ _ _) bat@(Batiment _ _ _ _ _ _ _) = (unite, bat)

--prop_pre_actionRaffinerie  : vérifier que bat est une raffinerie appartenant à joueur
prop_pre_actionRaffinerie :: Unite -> Bool
prop_pre_actionRaffinerie (Unite _ _ (Collecteur n _) _ _ _) = n > 0
prop_pre_actionRaffinerie _ = False

prop_post_actionRaffinerie :: Unite -> Bool
prop_post_actionRaffinerie (Unite _ _ (Collecteur n _) _ _ _) = n == 0
prop_post_actionRaffinerie _ = False

invariant_actionRaffinerie :: Unite -> Bool
invariant_actionRaffinerie (Unite _ _ Combattant _ _ _) = False
invariant_actionRaffinerie (Unite _ _ (Collecteur _ _) _ _ _) = True
invariant_actionRaffinerie _ = False

creerBatiment :: Joueur -> String -> Environement -> Coord -> Int -> Int -> Int -> (Bool, Environement)
creerBatiment j@(Joueur idj name credit ener) typeBat env@(Environement _ _ _ bats _) c idglobal prixbat pvbat =
    if (credit - prixbat) >=0 then 
        let t = btinitialisation typeBat in 
        case t of 
            Nothing -> (False, env)
            Just tnew ->
                let bat = Batiment { coordb = c
                                , propriob = idj
                                , typeb = tnew
                                , idb = BatId idglobal
                                , pvb = pvbat
                                , prixb = prixbat
                                , utilisable =True}
                    newBats = M.insert (BatId idglobal) bat bats
                    newEnv = env {joueurs = [(Joueur idj name (credit-prixbat) ener)], batiments = newBats }
                in (True, newEnv)
    else (False,env)

afficher :: String -> IO Bool
afficher chaine = do
    putStrLn chaine
    return True


creerUnite:: Joueur -> String -> Environement -> Coord -> Int -> Int -> Int -> (Bool, Environement)
creerUnite j@(Joueur idj name credit ener) typeUni env@(Environement _ _ unis _ _) c idglobal prixuni pvuni =
    let essa = afficher typeUni in 
    let t = uniinitialisation typeUni in 
        case t of 
            Nothing -> (False, env)
            Just tnew ->
                let uni = Unite { coordu= c
                                    ,propriou=idj,
                                    typeu= tnew,
                                    idu=UniteId idglobal,
                                    pvu=pvuni, -- pv si 0 => destruction 
                                    ordres= Pause}
                    newunis = M.insert (UniteId idglobal) uni unis
                    newEnv = env {joueurs = [(Joueur idj name (credit-prixuni) ener)],unites = newunis}
                    in (True, newEnv)



prop_pre_creerBatiment1::Environement-> Coord->Bool
prop_pre_creerBatiment1 env@(Environement _ carte _ _ _) coord = let t = getCase coord carte in 
    case t of 
    Just Herbe -> True 
    _ -> False

prop_pre_creerBatiment2 :: Environement -> Coord -> Bool
prop_pre_creerBatiment2 env@(Environement _ carte unis bats _) coord = 
    let res1 = cherche_Case_Batiment coord bats 
        res2 = cherche_Case_Unite coord unis 
    in case res1 of 
        [] -> case res2 of 
                [] -> True 
                _  -> False
        _  -> False


prop_post_creerBatiment1::Environement -> Coord -> String -> Bool
prop_post_creerBatiment1 env@(Environement _ _ _ bats _) coord ty = 
    case (cherche_Case_Batiment coord bats) of 
    b:[] -> let typ = btype b in if(typ==ty) then True else False
    _ -> False

prop_post_creerBatiment2::Environement->Environement -> Coord -> String -> Bool
prop_post_creerBatiment2 env@(Environement j carte unis bats _) envnew@(Environement newj newcarte newunis newbats _) coord ty = 
    if ((j==newj && carte==newcarte) && unis==newunis) then let keys1 = M.keysSet bats in
       let keys2 = M.keysSet newbats
    in (keys1 `S.isSubsetOf` keys2)
        else False

--invariant_creerBatiment::Environement-> Bool

cherche_Joueur_Batiment::Joueur -> M.Map BatId Batiment -> [Batiment]
cherche_Joueur_Batiment joueur batiments =
    map snd $ filter (\(_, bat) -> caseBatJoueur bat joueur) (M.toList batiments)
  where
    caseBatJoueur(Batiment _ p _ _ _ _ _) joueur = p == (jid joueur)

cherche_Joueur_Unite::Joueur -> M.Map UniteId Unite -> [Unite]
cherche_Joueur_Unite joueur unis=
    map snd $ filter (\(_, uni) -> caseUniteJoueur uni joueur) (M.toList unis)
  where
    caseUniteJoueur(Unite _ p _ _ _ _) joueur = p == (jid joueur)

actionProductionEnergie::Environement->Joueur->Int
actionProductionEnergie env@(Environement _ _ _ bats _) joueur =  
    let listebats=cherche_Joueur_Batiment joueur bats in 
    
    foldl (\res bat -> (caseBatJoueur bat joueur)+res) 0 listebats
  where
    caseBatJoueur(Batiment _ _ t _ _ _ _) joueur = case t of 
                                            QG n -> n 
                                            Centrale n -> n
                                            _-> 0

--prop_pre_actionProductionEnergie
--prop_post_actionProductionEnergie
--invariant_actionProductionEnergie

actionConsommationEnergie::Environement->Joueur->Int
actionConsommationEnergie env@(Environement _ _ _ bats _) joueur =  
    let listebats=cherche_Joueur_Batiment joueur bats in 
    
    foldl (\res bat -> (caseBatJoueur bat joueur)+res) 0 listebats
  where
    caseBatJoueur(Batiment _ _ t _ _ _ _) joueur = case t of 
                                            Raffinerie n _ -> n
                                            Usine n u temp etat-> n
                                            _-> 0

--prop_pre_actionConsommationEnergie
--prop_post_actionConsommationEnergie
--invariant_actionConsommationEnergie

stopperEnergie :: Environement -> Joueur -> Environement
stopperEnergie env@(Environement j carte unis bats enns) joueur = 
  let prod = actionProductionEnergie env joueur 
      conso = actionConsommationEnergie env joueur 
  in 
    if (prod < conso) then env else 
      let newbats = foldl (\acc (batid, bat) ->
                            let newBat = if (propriob bat == jid joueur) 
                                          then bat { utilisable = False } 
                                          else bat
                            in M.insert batid newBat acc) M.empty (M.toList bats)
      in Environement j carte unis newbats enns


cherche_fermeture_bats::[Batiment]->[BatId]
cherche_fermeture_bats bats =
    foldl (\res bat -> case (caseBatDest bat) of
                        Just idn -> idn:res
                        _ -> res) [] bats
    where
        caseBatDest (Batiment _ _ _ id pv _ _)= case pv of 
                                                0 -> Just id
                                                _-> Nothing

actionFermetureBatiment :: Environement -> Joueur -> Environement
actionFermetureBatiment env@(Environement j c u bats enns) joueur =  
    let listebats = cherche_Joueur_Batiment joueur bats in
    let listeferme = cherche_fermeture_bats listebats
    in foldl (\acc bId -> Environement j c u (M.delete bId bats) enns) env listeferme


-- vérifie que le joueur a au moins une unité
prop_pre_actionFermetureBatiment::Environement->Joueur->Bool
prop_pre_actionFermetureBatiment env@(Environement j c u bats _) joueur =
    let listebats = cherche_Joueur_Batiment joueur bats in
        case listebats of 
            [] -> False
            _ -> True

-- vérifie que les unités du joueur ont tous des pv > 0 
prop_post_actionFermetureBatiment::Environement->Joueur->Bool
prop_post_actionFermetureBatiment env@(Environement j c u bats _) joueur =
    let listebats = cherche_Joueur_Batiment joueur bats in
    let listeferme = cherche_fermeture_bats listebats in 
        case listeferme of 
            [] -> True
            _ -> False

--invariant_actionFermetureBatiment
                             
cherche_qg_joueur::[Batiment]->Bool
cherche_qg_joueur bats =
    foldl (\res bat -> (caseBatDest bat)|| res) False bats
    where
        caseBatDest (Batiment _ _ t _ _ _ _)= case t of 
                                                QG n -> True
                                                _-> False

verificationDestruction::Environement->Environement
verificationDestruction env@(Environement joueurs carte unis bats enns) =
    foldl (\acc joueur@(Joueur idj _ _ _)-> let listebats = cherche_Joueur_Batiment joueur bats in
                case (cherche_qg_joueur listebats) of
                True -> let newjoueurs =  filter (\j@(Joueur id name credit _)->
                                        id /= idj) joueurs in
                        
                        let listeunite=cherche_Joueur_Unite joueur unis in 
                        let newunis = foldl (\acc uni@(Unite _ _ _ idu _ _) -> (M.delete idu acc)) unis listeunite in
                        let newbats = foldl (\acc bat@(Batiment _ _ _ idb _ _ _) -> (M.delete idb acc)) bats listebats
                        in 
                        Environement newjoueurs carte newunis newbats enns
                False -> acc
             ) env joueurs
     
--prop_pre_verificationDestruction -> pas d'idée
--prop_post_verificationDestruction
--invariant_verificationDestruction -> pas d'idée

actionUsine::Environement ->Coord -> Int->String -> Environement
actionUsine env@(Environement _ _ _ bats _) c temp unite = 
    let newbats = foldl (\acc (bid,bat@(Batiment cb _ t _ _ _ uti))-> if (c==cb) then case t of 
                                                                                    Usine n un tem chaine -> let listebats =M.delete bid acc in M.insert bid bat{typeb=Usine n unite temp "en cours"} acc
                                                                                    _ -> acc
                                                                    else acc) bats (M.toList bats)
    in env{batiments=newbats}

actionFinUsine :: Environement -> Joueur -> Coord -> Coord -> Int ->Int-> (Bool,Environement)
actionFinUsine env@(Environement _ _ _ bats _) joueur c newcoord temp idg =
    let var = takeFirstBat (map snd $ filter (\(_, bat) -> usineCoordJoueur bat) (M.toList bats))
                                        
                                        where usineCoordJoueur (Batiment cu p t _ _ _ _) = cu == c && (case t of
                                                                Usine _ _ _ _ -> True
        
                                                                _ -> False)
        Batiment co po tp ido _ _ _ = case var of 
                                        Just res -> res
                                        Nothing -> error ("pas d'usine aux coordonées")
        Usine n unite temp chaine = tp
   in
   if chaine == "terminé"
       then let newbats = foldl (\acc (bid, bat) -> if ido == bid
                                                   then let listebats = M.delete bid bats
                                                            in M.insert bid (bat {typeb = Usine n unite 0 "vide"}) acc
                                                   else acc) bats (M.toList bats)
                (true,newenv) = creerUnite joueur unite env newcoord idg 0 10
            in (true,newenv{batiments = newbats})
    else (False,env)



prop_pre_actionUsine1::Batiment->Joueur->Bool --verifier que c est une usine et qu elle appartient à joueur
prop_pre_actionUsine1 bat@(Batiment _ pb typ _ _ _ _) joueur = case typ of
                                                                Usine _ _ _ _ -> (pb==jid joueur)
                                                                _ -> False
prop_pre_actionUsine2 ::Batiment->Bool --l'etat n'est pas "en cours ou terminé"
prop_pre_actionUsine2 bat@(Batiment _ _ (Usine n u temp "en cours") _ _ _ _)=False 
prop_pre_actionUsine2 bat@(Batiment _ _ (Usine n u temp "terminé") _ _ _ _)=False 
prop_pre_actionUsine2 bat@(Batiment _ _ (Usine n u temp "vide") _ _ _ _)=True

prop_post_actionUsine ::Batiment->Bool --l'etat est "en cours"
prop_post_actionUsine bat@(Batiment _ _ (Usine n u temp "en cours") _ _ _ _)=True
prop_post_actionUsine bat@(Batiment _ _ (Usine n u temp "terminé") _ _ _ _)=False 
prop_post_actionUsine bat@(Batiment _ _ (Usine n u temp "vide") _ _ _ _)=False

invariant_actionUsine ::Batiment->Batiment-> Bool --n et temps identique
invariant_actionUsine bat1@(Batiment _ _ (Usine n1 _ temp1 _) _ _ _ _) bat2@(Batiment _ _ (Usine n2 _ temp2 _) _ _ _ _) = if(n1==n2 && temp1==temp2) then True else False

recupUsine :: Batiment -> Joueur -> String
recupUsine bat@(Batiment _ _ (Usine n u temp _) _ _ _ _) joueur =
    let newBat = bat { typeb = Usine n u temp "vide" }
    in u

prop_pre_recupUsine1::Batiment->Joueur->Bool --verifier que c est une usine et qu elle appartient à joueur
prop_pre_recupUsine1 bat@(Batiment _ pb typ _ _ _ _) joueur = case typ of
                                                                Usine _ _ _ _ -> (pb==jid joueur)
                                                                _ -> False

prop_pre_recupUsine2 ::Batiment->Bool --l'etat est "terminé"
prop_pre_recupUsine2 bat@(Batiment _ _ (Usine n u temp "en cours") _ _ _ _)=False
prop_pre_recupUsine2 bat@(Batiment _ _ (Usine n u temp "terminé") _ _ _ _)=True
prop_pre_recupUsine2 bat@(Batiment _ _ (Usine n u temp "vide") _ _ _ _)=False

prop_post_recupUsine ::Batiment->Bool --l'etat est "vide"
prop_post_recupUsine bat@(Batiment _ _ (Usine n u temp "en cours") _ _ _ _)=False
prop_post_recupUsine bat@(Batiment _ _ (Usine n u temp "terminé") _ _ _ _)=False
prop_post_recupUsine bat@(Batiment _ _ (Usine n u temp "vide") _ _ _ _)=True

invariant_recupUsine ::Batiment->Batiment-> Bool --n et temps identique
invariant_recupUsine bat1@(Batiment _ _ (Usine n1 _ temp1 _) _ _ _ _) bat2@(Batiment _ _ (Usine n2 _ temp2 _) _ _ _ _) = if(n1==n2 && temp1==temp2) then True else False


------------------
-- PARTIE UNITE --
------------------

cherche_fermeture_unite::[Unite]->[UniteId]
cherche_fermeture_unite unis =
    foldl (\res uni -> case (caseUniDest uni) of
                        Just idn -> idn:res
                        _ -> res) [] unis
    where
        caseUniDest (Unite _ _ _ id pv _)= case pv of 
                                                0 -> Just id
                                                _-> Nothing

actionFermetureUnite :: Environement -> Joueur -> Environement
actionFermetureUnite env@(Environement j c unis b enns) joueur =  
    let listeunis = cherche_Joueur_Unite joueur unis in
    let listeferme = cherche_fermeture_unite listeunis
    in foldl (\acc uId -> Environement j c (M.delete uId unis) b enns) env listeferme

-- vérifie que le joueur a au moins une unité
prop_pre_actionFermetureUnite::Environement->Joueur->Bool
prop_pre_actionFermetureUnite env@(Environement j c unis b _) joueur =
    let listeunis = cherche_Joueur_Unite joueur unis in
        case listeunis of 
            [] -> False
            _ -> True

-- vérifie que les unités du joueur ont tous des pv > 0 
prop_post_actionFermetureUnite::Environement->Joueur->Bool
prop_post_actionFermetureUnite env@(Environement j c unis b _) joueur =
    let listeunis = cherche_Joueur_Unite joueur unis in
    let listeferme = cherche_fermeture_unite listeunis in 
        case listeferme of 
            [] -> True
            _ -> False

--invariant_actionFermetureBatiment -> pas d'idée ? 
data Direction = Bas
                | Haut
                | Gauche
                | Droit
                deriving(Eq,Show)

actionDeplacerUnite :: Environement -> Unite -> Direction -> Environement
actionDeplacerUnite env@(Environement j carte unis bats enns) uni@(Unite coord@(Coord x y) _ _ _ _ _) direction =
  let coordTerrain = case direction of
                       Bas -> Coord x (y+1)
                       Haut -> Coord x (y-1)
                       Gauche -> Coord (x-1) y
                       Droit -> Coord (x+1) y
  in
  let listebats = cherche_Case_Batiment coordTerrain bats
      listeunis = cherche_Case_Unite coordTerrain unis
      listeenns = trouver_ennemis enns coordTerrain
  in
  case listebats of
    [] -> case listeunis of
            [] -> case listeenns of
                []->env {unites = deplace_Unite uni coordTerrain unis}
                _ -> env
            _ -> env
    _ -> env {unites = unis, batiments = bats}
  where
    deplace_Unite u c unis = 
        let updatedUnite = u {coordu = c}
        in M.insert (idu u) updatedUnite $ M.filter (\x -> coordu x /= c) unis

-- vérifier que la direction est possible => case existe 
prop_pre_actionDeplacerUnite::Environement-> Unite -> Direction->Bool
prop_pre_actionDeplacerUnite env@(Environement _ carte unis _ _) uni@(Unite coord@(Coord x y) _ _ _ _ _) direction=
    let coordTerrain = case direction of
                       Bas -> Coord x (y-1)
                       Haut -> Coord x (y+1)
                       Gauche -> Coord (x-1) y
                       Droit -> Coord (x+1) y
                      
  in let terrain = getCase coordTerrain carte in 
    case terrain of 
        Just r -> True
        Nothing -> False

--verifier que l'unité a bien été déplacé sur la bonne case
prop_post_actionDeplacerUnite::Environement -> Environement-> Unite -> Direction->Bool
prop_post_actionDeplacerUnite env1@(Environement _ _ unis1 _ _) env2@(Environement _ _ unis2 _ _) uni@(Unite _ _ _ idu _ _) direction=
    let (Coord x y) = foldl (\acc (id,unit@(Unite c _ _ _ _ _))-> if(id==idu) then c else acc) (Coord 0 0) (M.toList unis2) in
    let coordTerrain = case direction of
                        Bas -> Coord x (y+1)
                        Haut -> Coord x (y-1)
                        Gauche -> Coord (x+1) y
                        Droit -> Coord (x-1) y
                    

    in let coordenv1 = foldl (\acc (id,unit@(Unite c _ _ _ _ _))-> if(id==idu) then c else acc) (Coord 0 0) (M.toList unis1) 
    in (coordenv1==coordTerrain) 


myAbs :: Int -> Int
myAbs val =
  if (val < 0)
    then val * (-1)
    else val

calculDirection::Coord->Coord->Maybe Direction
calculDirection (Coord x1 y1) (Coord x2 y2) =
    if(x1==x2 && y1==y2) then Nothing
    else 
        let newx= x2-x1 
            newy = y2-y1 in 
            if( myAbs newx > myAbs newy) then if(newx<0) then Just Gauche
                                                     else Just Droit
            else 
                if(newy<0) then Just Haut 
                            else Just Bas

coordRaffinerie :: M.Map BatId Batiment -> JoueurId -> [Coord]
coordRaffinerie bats joueur = 
    map coord $ filter (\bat -> rafCoordJoueur bat joueur) (M.elems bats)
  where
    rafCoordJoueur (Batiment c p (Raffinerie _ _) _ _ _ _) j = p == j
    coord (Batiment c _ _ _ _ _ _) = c


trouver_raffinerie::M.Map BatId Batiment->JoueurId->[Batiment]
trouver_raffinerie bats joueur =
     map snd $ filter (\(_, bat) -> rafCoordJoueur bat joueur) (M.toList bats)
  where
    rafCoordJoueur (Batiment c p t _ _ _ _) j= p == j && (case t of
                                                            Raffinerie r1 r2-> True
                                                            _->False)
    

modifier_credit :: JoueurId -> Environement -> Environement
modifier_credit idj env@(Environement joueurs carte unis bats enns) =
    let raf= trouver_raffinerie bats idj
        newcred = foldl (\acc bat@(Batiment c p (Raffinerie _ ressource) _ _ _ _)-> acc+ressource) 0 raf
    in 
    let updated_joueurs = map (\j@(Joueur id name cred ener) ->
                                if (id == idj)
                                    then Joueur id name (cred + newcred) ener
                                    else j) joueurs
        updated_bats = foldl (\acc (idb,bat@(Batiment _ _ t id _ _ _))->
                                let batiment = case t of 
                                                Raffinerie n r -> bat{typeb=Raffinerie n 0}
                                                _->  bat
                                in M.insert id batiment acc) M.empty (M.toList bats)         
    in Environement updated_joueurs carte unis updated_bats enns


situer_A_une_Case::Coord->Coord->Bool
situer_A_une_Case (Coord x1 y1) (Coord x2 y2)=
    if( (x1==x2 && y1==y2)|| (x1==x2+1 && y1== y2+1) || (x1==x2+1 && y1== y2-1) || (x1==x2-1 && y1== y2-1) || (x1==x2-1 && y1== y2+1)) then
        True
    else False

{- SI ON CHOISIT D'AVOIR PLUSIEURS JOUEURS => LES ENNEMIS SONT LES AUTRES JOUEURS
--------------------------------------------------------------------------------
trouver_ennemis::M.Map UniteId Unite-> Coord -> JoueurId-> [Unite]
trouver_ennemis unis coord joueur = 
    map snd $ filter (\(_, bat) -> rafCoordJoueur bat joueur) (M.toList unis)
  where
    rafCoordJoueur (Unite c p _ _ _ _) j= p /= j && (situer_A_une_Case coord c) -}

trouver_ennemis::[Coord]-> Coord ->[Coord]
trouver_ennemis ennemis coord = 
    filter (\ ennemi -> rafCoordJoueur ennemi) ennemis
  where
    rafCoordJoueur c = situer_A_une_Case coord c

bonOrdreType::TypeUnite -> TypeOrdres -> Bool
bonOrdreType tuni tor = 
    case tuni of 
        Combattant -> case tor of 
                        Collecter -> False
                        Deplacer ct t -> True
                        Patrouiller c1 c2 -> True
                        Attaquer ca t -> True
                        PoserRaffinerie -> False
                        Pause -> True
                        Recherche -> False
        Collecteur _ _ -> case tor of 
                        Collecter -> True
                        Deplacer ct t -> True
                        Patrouiller c1 c2 -> False
                        Attaquer ca t -> False
                        PoserRaffinerie -> True
                        Pause -> True
                        Recherche -> True


donnerOrdre::Environement -> Coord-> TypeOrdres -> Environement
donnerOrdre env@(Environement joueurs carte unis bats enns) c ordre = 
    let uni = cherche_Case_Unite c unis in 
        case uni of
            [] -> error ("pas d'unité à ces coordonnées")
            u1:[]-> let (Unite cu pu tu idu pvu ordresu) = u1 
                        newordre = case ordre of 
                                Collecter -> Collecter
                                Deplacer ct t -> Deplacer ct ordresu
                                Patrouiller c1 c2 -> Patrouiller c1 c2
                                Attaquer ca t -> Attaquer ca ordresu
                                PoserRaffinerie -> PoserRaffinerie
                                Pause -> Pause
                                Recherche -> Recherche
                        estPossible = bonOrdreType tu newordre in 
                            case estPossible of
                                True -> let newliste = M.delete idu unis in Environement joueurs carte (M.insert idu u1{ordres=newordre} newliste) bats enns
                                False -> env
            u1:u2 -> error ("trop d'unité sur ces coordonnées")

getCoordonneesVoisines :: Coord -> Carte -> M.Map UniteId Unite-> M.Map BatId Batiment->[Coord] -> [Coord]
getCoordonneesVoisines c@(Coord x y) carte@(Carte xmax ymax contenu) unis bats ennemis=
  let voisins = [(Coord (x - 1)  y), (Coord (x + 1)  y), (Coord x (y - 1)), (Coord x (y + 1))]
  in filter (\c1@(Coord x1 y1)-> (estCaseLibre unis bats c1)&&(estPresentDansListe c1 ennemis)&&(x1 > -1 && x1 < (xmax +1) && y1 > -1 && y1 < (ymax +1))) voisins

estCaseLibre :: M.Map UniteId Unite-> M.Map BatId Batiment -> Coord -> Bool
estCaseLibre unis bats coord =
  let listeunis = cherche_Case_Unite  coord unis 
      listebats = cherche_Case_Batiment coord bats
      in 
    case listeunis of 
        []-> case listebats of
                []-> True 
                _ -> False
        _-> False

estPresentDansListe :: Coord -> [Coord] -> Bool
estPresentDansListe _ [] = True
estPresentDansListe coord (x:xs)
  | coord == x = False
  | otherwise = estPresentDansListe coord xs

modifCoordRaf::Environement -> [Coord] -> [Coord] 
modifCoordRaf env@(Environement joueurs carte@(Carte l h contenu) unis bats enns) craf =
    let coordraf = case (takeFirstCoord craf) of
                    Just fc -> fc
                    Nothing -> error ("pas de coordonné") in 
    getCoordonneesVoisines coordraf carte unis bats enns


getCoordonneesAttaquerUnite :: Coord -> Carte -> M.Map UniteId Unite -> Maybe Coord
getCoordonneesAttaquerUnite c@(Coord x y) carte@(Carte xmax ymax contenu) unis =
    let voisins = [(Coord (x - 1) y), (Coord (x + 1) y), (Coord x (y - 1)), (Coord x (y + 1))]
    in case filter (\c1@(Coord x1 y1) -> case cherche_Case_Unite c1 unis of 
                                            u1:_ -> True
                                            _ -> False) voisins of
        u1:_ -> Just u1
        _ -> Nothing

getCoordonneesAttaquerBatiment :: Coord -> Carte -> M.Map BatId Batiment -> Maybe Coord
getCoordonneesAttaquerBatiment c@(Coord x y) carte@(Carte xmax ymax contenu) bats =
    let voisins = [(Coord (x - 1) y), (Coord (x + 1) y), (Coord x (y - 1)), (Coord x (y + 1))]
    in case filter (\c1@(Coord x1 y1) -> case cherche_Case_Batiment c1 bats of
                                            b1:_ -> True
                                            _ -> False) voisins of
        b1:_ -> Just b1
        _ -> Nothing


getUniteCoord:: M.Map UniteId Unite -> Coord -> Maybe Unite
getUniteCoord unites c = 
    let liste = filter (\uni@(Unite cu pu tu idu pvu ordresu) -> c==cu ) (M.elems unites) in
        case liste of
            u1:reste -> Just u1
            _ -> Nothing

getBatCoord:: M.Map BatId Batiment-> Coord -> Maybe Batiment
getBatCoord batiments c = 
    let liste = filter (\elem@(Batiment cb pb tb idb pvb prixb utilisable) -> c==cb ) (M.elems batiments) in
        case liste of
            u1:reste -> Just u1
            _ -> Nothing

mouvemenEnnemis::Environement -> Environement
mouvemenEnnemis env@(Environement joueurs carte@(Carte l h contenu) unis bats enns) = 
    foldl (\acc c1-> let uni = getCoordonneesAttaquerUnite c1 carte unis 
                        in case uni of
                            Just elem->  case getUniteCoord unis elem of 
                                            Just uni@(Unite cu pu tu idu pvu ordresu) -> let listeunis= M.delete idu unis in Environement joueurs carte (M.insert idu uni{pvu=pvu -1} listeunis) bats enns
                                            Nothing -> acc
                            Nothing -> let bat = getCoordonneesAttaquerBatiment c1 carte bats 
                                        in case bat of
                                            Just elem -> case getBatCoord bats elem of 
                                                            Just bat@(Batiment cb pb tb idb pvb prixb utilisable) -> let listebats= M.delete idb bats in Environement joueurs carte unis (M.insert idb bat{pvb=pvb -1} listebats) enns
                                                            Nothing -> error ("pb de coordonnée")
                                            Nothing-> acc) env enns 
{- AUTRE VERSION DE FERMETURE UNITE ET BATIMENT 
------------------------------------------------
mortUniteBat::Environement -> Environement
mortUniteBat env@(Environement joueurs carte@(Carte l h contenu) unis bats enns) = 
     let newenv = foldl (\acc@(Environement _ _ unisAcc _ _) ( uId, uni@(Unite cu pu tu idu pvu ordresu))-> if (pvu ==0) then let listeunis= M.delete idu unisAcc in Environement joueurs carte listeunis bats enns else acc) env (M.toList unis) 
     in foldl (\acc@(Environement _ _ _ batsAcc _) ( bId, bat@(Batiment cb pb tb idb pvb prixb utilisable))-> if (pvb ==0) then let listebats= M.delete idb batsAcc in Environement joueurs carte unis listebats enns else acc) newenv (M.toList bats)-}

supprimerElement :: Coord -> [Coord] -> [Coord]
supprimerElement _ [] = [] 
supprimerElement cen (x:xs)
  | x == cen = xs 
  | otherwise = x : supprimerElement cen xs 

etape::Environement->Unite->Environement
etape env@(Environement joueurs carte@(Carte l h contenu) unis bats enns) uni@(Unite cu pu tu idu pvu ordresu) =
    case ordresu of
        Deplacer c ordrebase ->   case tu of
                                    Combattant -> case (trouver_ennemis enns cu) of
                                                    cen:reste ->let listeunis= M.delete idu unis in 
                                                        Environement joueurs carte (M.insert idu uni{ordres=Attaquer cen (Deplacer c ordrebase)} listeunis) bats enns
                                                    [] ->let direction = calculDirection cu c in case direction of 
                                                                                                Just d -> actionDeplacerUnite env uni d
                                                                                                Nothing -> case ordrebase of
                                                                                                            Patrouiller c1 c2 -> let listeunis= M.delete idu unis in 
                                                                                                                                Environement joueurs carte (M.insert idu uni{ordres=Patrouiller c2 c1} listeunis) bats enns
                                                                                                            _ -> let listeunis= M.delete idu unis in 
                                                                                                                Environement joueurs carte (M.insert idu uni{ordres=Pause} listeunis) bats enns
                                    Collecteur _ _ -> let direction = calculDirection cu c in case direction of 
                                                                                                Just d -> actionDeplacerUnite env uni d
                                                                                                Nothing -> case ordrebase of
                                                                                                            Collecter -> let listeunis= M.delete idu unis in 
                                                                                                                        Environement joueurs carte (M.insert idu uni{ordres=PoserRaffinerie} listeunis) bats enns
                                                                                                            Recherche -> let listeunis= M.delete idu unis in 
                                                                                                                        Environement joueurs carte (M.insert idu uni{ordres=Collecter} listeunis) bats enns
                                                                                                            _ -> let listeunis= M.delete idu unis in 
                                                                                                                Environement joueurs carte (M.insert idu uni{ordres=Pause} listeunis) bats enns
        Collecter -> let (Collecteur n max) = tu in if(n==max) then let coordraf = coordRaffinerie bats pu 
                                                                        newcoordraf = modifCoordRaf env coordraf in
                                                                    let listeunis= M.delete idu unis in 
                                                                        case newcoordraf of 
                                                                            raf:[]-> Environement joueurs carte (M.insert idu uni{ordres=Deplacer raf Collecter} listeunis) bats enns
                                                                            _ -> env 
                                                    else 
                                                        let carre = getCase cu carte in 
                                                            case carre of
                                                                Just (Ressource 0)-> let listeunis= M.delete idu unis in 
                                                                                        let (Carte l h contenu)=carte
                                                                                            (Coord x y )=cu in
                                                                                        if(x>0 && x<l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord (x+1) y) Recherche} listeunis) bats enns
                                                                                        else if(y>0 && y<l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord x (y+1)) Recherche} listeunis) bats enns
                                                                                            else if(x==l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord (x-1) y) Recherche} listeunis) bats enns
                                                                                                else Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord x (y-1)) Recherche} listeunis) bats enns
                                                                Just (Ressource r) -> let valres = let res=n+r in if res>max then max else res
                                                                                          newcarte= M.delete cu contenu
                                                                                          listeunis= M.delete idu unis             
                                                                                in Environement joueurs (Carte h l (M.insert cu (Ressource (r-(valres-n))) contenu )) (M.insert idu uni{typeu=Collecteur valres max} listeunis) bats enns
                                                                
                                                                _ -> let listeunis= M.delete idu unis in 
                                                                    let (Carte l h contenu)=carte
                                                                        (Coord x y )=cu in
                                                                    if(x>0 && x<l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord (x+1) y) Recherche} listeunis) bats enns
                                                                    else if(y>0 && y<l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord x (y+1)) Recherche} listeunis) bats enns
                                                                        else if(x==l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord (x-1) y) Recherche} listeunis) bats enns
                                                                            else Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord x (y-1)) Recherche} listeunis) bats enns


        PoserRaffinerie -> let raffineries = trouver_raffinerie bats pu
                            in let (newuni,newbat) = case takeFirstBat (filter (\(Batiment cb _ _ _ _ _ _) -> cb == cu) raffineries) of
                                            Nothing -> error "Pas de raffinerie pour ce joueur !"
                                            Just r -> actionRaffinerie uni r 
                            in let listeunis= M.delete idu unis
                                   listebats = M.delete (bid newbat) bats
                            in Environement joueurs carte (M.insert idu newuni{ordres=Pause} listeunis) (M.insert (bid newbat) newbat listebats) enns

        
        Patrouiller c1 c2 -> let ennemis=trouver_ennemis enns cu in case ennemis of
                                                                    [] -> let listeunis= M.delete idu unis in 
                                                                        Environement joueurs carte (M.insert idu uni{ordres=Deplacer c1 (Patrouiller c1 c2)} listeunis) bats enns
                                                                    cen:reste-> let listeunis= M.delete idu unis in 
                                                                        Environement joueurs carte (M.insert idu uni{ordres=Attaquer cen (Patrouiller c1 c2)} listeunis) bats enns

        Attaquer c ordrebase -> let ennemis = cherche_Case_Ennemis c enns in 
                                case ennemis of 
                                    []-> let listeunis= M.delete idu unis in 
                                        Environement joueurs carte (M.insert idu uni{ordres=Pause} listeunis) bats enns
                                    cen:reste -> let newenns = supprimerElement cen enns in 
                                        Environement joueurs carte unis bats newenns
        
        Pause -> env


miseAjourEnv :: Environement -> Int -> Environement
miseAjourEnv env@(Environement joueurs carte unis bats enns) temp =
    let unites = cherche_fermeture_unite (M.elems unis)
        newunis = foldl (\acc uid -> M.delete uid acc) unis unites
        batiments = cherche_fermeture_bats (M.elems bats)
        newbats = foldl (\acc bid -> M.delete bid acc) bats batiments
        newfinalbats = foldl (\acc (bid, bat@(Batiment _ _ t _ _ t1 _)) -> case t of
                Usine ener tu t1' chaine ->
                    if (temp - t1') > 600
                        then let newbats = M.delete bid acc in
                                M.insert bid (bat{typeb = Usine ener tu 0 "terminé"}) acc
                        else acc
                _ -> acc
                ) newbats (M.toList newbats)
    in env{unites = newunis, batiments = newfinalbats}

