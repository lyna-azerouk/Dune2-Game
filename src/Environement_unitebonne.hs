module Environement_unitebonne where
import qualified Data.Map.Strict as M
import Data.Ord
import Data.List
import Carte
import Control.Monad (when)
import Control.Monad (foldM)
import System.Random
import qualified Data.Set as S

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
    | Usine Int Unite Int String-- Int = energie_conso , Unite = unite produite et Int = temps de production
    | Centrale Int -- Int = energie_prod


data Batiment = Batiment{
    coordb::Coord,
    propriob::JoueurId,
    typeb:: TypeBatiment,
    idb::BatId,
    pvb::Int, -- pv si 0 => destruction 
    prixb::Int, -- coût du bâtiment en crédits
    utilisable::Bool
}

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
    Raffinerie r ->"raffinerie"
    Usine r1 u r2 etat ->"usine"
    Centrale r -> "centrale"
    _-> "error pas de type bâtiment"

btinitialisation:: String -> Maybe TypeBatiment
btinitialisation typeb= case typeb of
    "raffinerie" -> Just (Raffinerie 10)
    --"usine" -> Usine 10 Unite 0
    "centrale" -> Just (Centrale 15)
    _-> Nothing

newtype UniteId = UniteId Int 
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
    | Deplacer Direction TypeOrdres
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
}

instance Eq Unite where 
    (Unite c1 p1 t1 i1 pv1 or1) == (Unite c2 p2 t2 i2 pv2 or2)= (c1==c2) && (p1==p2) && (t1==t2) && (i1==i2) && (pv1==pv2) && (or1==or2)


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
    caseBatimentCoord (Batiment c _ _ _ _ _ _) coordb = c == coordb

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
    all (\(_, bat@( Batiment _ propriob _ _ _ _ _)) ->case chercheJoueur joueurs propriob of
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
creerListeBatConst bats res = foldl (\acc bat@(Batiment _ _ _ idb _ _ _) -> M.insert idb bat acc) res bats


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

---------------------
-- PARTIE BATIMENT --
---------------------


actionRaffinerie :: Unite -> Batiment -> (Unite, Int)
actionRaffinerie unite@(Unite _ _ (Collecteur n) _ _ _) bat =
  let nbrRessources = ressouceCollecteur unite
  in (unite { typeu = Collecteur 0 }, nbrRessources)

--prop_pre_actionRaffinerie  : vérifier que bat est une raffinerie appartenant à joueur
prop_pre_actionRaffinerie :: Unite -> Bool
prop_pre_actionRaffinerie (Unite _ _ (Collecteur n) _ _ _) = n > 0
prop_pre_actionRaffinerie _ = False

prop_post_actionRaffinerie :: Unite -> Bool
prop_post_actionRaffinerie (Unite _ _ (Collecteur n) _ _ _) = n == 0
prop_post_actionRaffinerie _ = False

invariant_actionRaffinerie :: Unite -> Bool
invariant_actionRaffinerie (Unite _ _ Combattant _ _ _) = False
invariant_actionRaffinerie (Unite _ _ (Collecteur _) _ _ _) = True
invariant_actionRaffinerie _ = False

creerBatiment :: Joueur -> String -> Environement -> Coord -> Int -> Int -> Int -> (Bool, Environement)
creerBatiment j typeBat env@(Environement _ _ _ bats) c idglobal prixbat pvbat =
    let t = btinitialisation typeBat in 
    case t of 
        Nothing -> (False, env)
        Just tnew ->
            let bat = Batiment { coordb = c
                               , propriob = jid j
                               , typeb = tnew
                               , idb = BatId idglobal
                               , pvb = pvbat
                               , prixb = prixbat }
                newBats = M.insert (BatId idglobal) bat bats
                newEnv = env { batiments = newBats }
            in (True, newEnv)

prop_pre_creerBatiment1::Environement-> Coord->Bool
prop_pre_creerBatiment1 env@(Environement _ carte _ _) coord = let t = getCase coord carte in 
    case t of 
    Just Herbe -> True 
    _ -> False

prop_pre_creerBatiment2 :: Environement -> Coord -> Bool
prop_pre_creerBatiment2 env@(Environement _ carte unis bats) coord = 
    let res1 = cherche_Case_Batiment coord bats 
        res2 = cherche_Case_Unite coord unis 
    in case res1 of 
        [] -> case res2 of 
                [] -> True 
                _  -> False
        _  -> False


prop_post_creerBatiment1::Environement -> Coord -> String -> Bool
prop_post_creerBatiment1 env@(Environement _ _ _ bats) coord ty = 
    case (cherche_Case_Batiment coord bats) of 
    b:[] -> let typ = btype b in if(typ==ty) then True else False
    _ -> False

prop_post_creerBatiment2::Environement->Environement -> Coord -> String -> Bool
prop_post_creerBatiment2 env@(Environement j carte unis bats) envnew@(Environement newj newcarte newunis newbats) coord ty = 
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
actionProductionEnergie env@(Environement _ _ _ bats) joueur =  
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
actionConsommationEnergie env@(Environement _ _ _ bats) joueur =  
    let listebats=cherche_Joueur_Batiment joueur bats in 
    
    foldl (\res bat -> (caseBatJoueur bat joueur)+res) 0 listebats
  where
    caseBatJoueur(Batiment _ _ t _ _ _ _) joueur = case t of 
                                            Raffinerie n -> n
                                            Usine n u temp etat-> n
                                            _-> 0

--prop_pre_actionConsommationEnergie
--prop_post_actionConsommationEnergie
--invariant_actionConsommationEnergie

stopperEnergie :: Environement -> Joueur -> Environement
stopperEnergie env@(Environement j carte unis bats) joueur = 
  let prod = actionProductionEnergie env joueur 
      conso = actionConsommationEnergie env joueur 
  in 
    if (prod < conso) then env else 
      let newbats = foldl (\acc (batid, bat) ->
                            let newBat = if (propriob bat == jid joueur) 
                                          then bat { utilisable = False } 
                                          else bat
                            in M.insert batid newBat acc) M.empty (M.toList bats)
      in Environement j carte unis newbats


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
actionFermetureBatiment env@(Environement j c u bats) joueur =  
    let listebats = cherche_Joueur_Batiment joueur bats in
    let listeferme = cherche_fermeture_bats listebats
    in foldl (\acc bId -> Environement j c u (M.delete bId bats)) env listeferme


-- vérifie que le joueur a au moins une unité
prop_pre_actionFermetureBatiment::Environement->Joueur->Bool
prop_pre_actionFermetureBatiment env@(Environement j c u bats) joueur =
    let listebats = cherche_Joueur_Batiment joueur bats in
        case listebats of 
            [] -> False
            _ -> True

-- vérifie que les unités du joueur ont tous des pv > 0 
prop_post_actionFermetureBatiment::Environement->Joueur->Bool
prop_post_actionFermetureBatiment env@(Environement j c u bats) joueur =
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
verificationDestruction env@(Environement joueurs carte unis bats) =
    foldl (\acc joueur@(Joueur idj _ _)-> let listebats = cherche_Joueur_Batiment joueur bats in
                case (cherche_qg_joueur listebats) of
                True -> let newjoueurs =  filter (\j@(Joueur id name credit)->
                                        id /= idj) joueurs in
                        
                        let listeunite=cherche_Joueur_Unite joueur unis in 
                        let newunis = foldl (\acc uni@(Unite _ _ _ idu _ _) -> (M.delete idu acc)) unis listeunite in
                        let newbats = foldl (\acc bat@(Batiment _ _ _ idb _ _ _) -> (M.delete idb acc)) bats listebats
                        in 
                        Environement newjoueurs carte newunis newbats
                False -> acc
             ) env joueurs
     
--prop_pre_verificationDestruction -> pas d'idée
--prop_post_verificationDestruction
--invariant_verificationDestruction -> pas d'idée

actionUsine::Batiment->Joueur->Unite->Batiment
actionUsine bat@(Batiment _ _ (Usine n u temp _) _ _ _ _) joueur unite = bat{typeb=Usine n unite temp "en cours"}

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

recupUsine :: Batiment -> Joueur -> Unite
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
actionFermetureUnite env@(Environement j c unis b) joueur =  
    let listeunis = cherche_Joueur_Unite joueur unis in
    let listeferme = cherche_fermeture_unite listeunis
    in foldl (\acc uId -> Environement j c (M.delete uId unis) b) env listeferme

-- vérifie que le joueur a au moins une unité
prop_pre_actionFermetureUnite::Environement->Joueur->Bool
prop_pre_actionFermetureUnite env@(Environement j c unis b) joueur =
    let listeunis = cherche_Joueur_Unite joueur unis in
        case listeunis of 
            [] -> False
            _ -> True

-- vérifie que les unités du joueur ont tous des pv > 0 
prop_post_actionFermetureUnite::Environement->Joueur->Bool
prop_post_actionFermetureUnite env@(Environement j c unis b) joueur =
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

actionDeplacerUnite :: Environement -> Unite -> Direction -> Environement
actionDeplacerUnite env@(Environement j carte unis bats) uni@(Unite coord@(Coord x y) _ _ _ _ _) direction =
  let coordTerrain = case direction of
                       Bas -> Coord x (y-1)
                       Haut -> Coord x (y+1)
                       Gauche -> Coord (x-1) y
                       Droit -> Coord (x+1) y
  in
  let listebats = cherche_Case_Batiment coordTerrain bats
      listeunis = cherche_Case_Unite coordTerrain unis
  in
  case listebats of
    [] -> case listeunis of
            [] -> env {unites = deplace_Unite uni coordTerrain unis}
            _ -> env
    _ -> env {unites = unis, batiments = bats}
  where
    deplace_Unite u c unis = 
        let updatedUnite = u {coordu = c}
        in M.insert (idu u) updatedUnite $ M.filter (\x -> coordu x /= c) unis

-- vérifier que la direction est possible => case existe 
prop_pre_actionDeplacerUnite::Environement-> Unite -> Direction->Bool
prop_pre_actionDeplacerUnite env@(Environement _ carte unis _) uni@(Unite coord@(Coord x y) _ _ _ _ _) direction=
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
prop_post_actionDeplacerUnite env1@(Environement _ _ unis1 _) env2@(Environement _ _ unis2 _) uni@(Unite _ _ _ idu _ _) direction=
    let (Coord x y) = foldl (\acc (id,unit@(Unite c _ _ _ _ _))-> if(id==idu) then c else acc) (Coord 0 0) (M.toList unis2) in
    let coordTerrain = case direction of
                        Bas -> Coord x (y+1)
                        Haut -> Coord x (y-1)
                        Gauche -> Coord (x+1) y
                        Droit -> Coord (x-1) y
                    

    in let coordenv1 = foldl (\acc (id,unit@(Unite c _ _ _ _ _))-> if(id==idu) then c else acc) (Coord 0 0) (M.toList unis1) 
    in (coordenv1==coordTerrain) 


invariant_actionDeplacerUnite::Unite -> Environement -> Bool
invariant_actionDeplacerUnite uni@(Unite _ pu tu idu pvu oru) env@( _ _ unis _) = 
    let (Coord x y) = foldl (\acc (id,unit@(Unite c _ _ _ _ _))-> if(id==idu) then c else acc) (Coord 0 0) (M.toList unis2) in

abs::Int-> Int
abs val = 
    if(val<0) then val*(-1)
    else val

calculDirection::Coord->Coord->Maybe Direction
calculDirection (Coord x1 y1) (Coord x2 y2) =
    if(x1==x2 && y1==y2) then Nothing
    else 
        let newx= x2-x1 
        newy = y2-y1 in 
            if( abs newx > abs newy) then if(newx<0) then Just Gauche
                                                     else Just Droit
            else 
                if(newy<0) then Just Haut 
                            else Just Bas

coordRaffinerie::M.Map BatId Batiment->JoueurId->Coord
coordRaffinerie bats joueur = 
    map coord $ filter (\(_, bat) -> RafCoordJoueur bat joueur) (M.toList bats)
  where
    RafCoordJoueur (Batiment c p _ _ _ _ _) j= p == j
    coord (Batiment c _ _ _ _ _ _) = c

trouver_raffinerie::M.Map BatId Batiment->JoueurId->batCoord
trouver_raffinerie bats joueur =
     map snd $ filter (\(_, bat) -> RafCoordJoueur bat joueur) (M.toList bats)
  where
    RafCoordJoueur (Batiment c p _ _ _ _ _) j= p == j
    

modifier_credit::JoueurId->Environement->Int->Environement
modifier_credit idj env@(joueurs carte unis bats) newcred=
    all (\ j@(id _ cred)-> if (id == idj) then j{credit=cred+ newcred}) joueurs in env 

situer_A_une_Case::Coord->Coord->Bool
situer_A_une_Case (Coord x1 y1) (Coord x2 y2)=
    if( (x1==x2 && y1==y2)|| (x1==x2+1 && y1== y2+1) || (x1==x2+1 && y1== y2-1) || (x1==x2-1 && y1== y-1) || (x1==x2-1 && y1== y2+1)) then
        True
    else False

trouver_ennemis::M.Map UniteId Unite-> Coord -> JoueurId-> [Unite]
trouver_ennemis unis coord joueur = 
    map snd $ filter (\(_, bat) -> RafCoordJoueur bat joueur) (M.toList unis)
  where
    RafCoordJoueur (Unite c p _ _ _ _) j= p != j && (situer_A_une_Case coord c)

-- il y aura plus tard un pb car on déplace l'unité aux coord de la raffinerie mais vu qu'il y a un batiment l'unité ne va pas aller dessus
etape::Unite -> Environement
etape env@(Environement joueurs carte unis bats) uni@(Unite cu pu tu idu pvu ordresu) =
    case ordresu of
        Deplacer c ordrebase ->   case tu of
                                    Combattant -> case (trouver_ennemis unis cu pu) of
                                                    (Unite cen _ _ _ _ _):reste ->let listeunis= M.delete idu unis in 
                                                        Environement joueurs carte (M.insert idu uni{ordres=Attaquer cen (Deplacer c ordrebase)} listeunis) bats
                                                    [] ->let direction = calculDirection cu c in case direction of 
                                                                                                Just d -> actionDeplacerUnite env uni direction
                                                                                                Nothing -> case ordrebase of
                                                                                                            Patrouiller c1 c2 -> let listeunis= M.delete idu unis in 
                                                                                                                                Environement joueurs carte (M.insert idu uni{ordres=Patrouiller c2 c1} listeunis) bats
                                                                                                            _ -> let listeunis= M.delete idu unis in 
                                                                                                                Environement joueurs carte (M.insert idu uni{ordres=Pause} listeunis) bats
                                    Collecteur _ _ -> let direction = calculDirection cu c in case direction of 
                                                                                                Just d -> actionDeplacerUnite env uni direction
                                                                                                Nothing -> case ordrebase of
                                                                                                            Collecter -> let listeunis= M.delete idu unis in 
                                                                                                                        Environement joueurs carte (M.insert idu uni{ordres=PoserRaffinerie} listeunis) bats
                                                                                                            Recherche -> let listeunis= M.delete idu unis in 
                                                                                                                        Environement joueurs carte (M.insert idu uni{ordres=Collecter} listeunis) bats
                                                                                                            _ -> let listeunis= M.delete idu unis in 
                                                                                                                Environement joueurs carte (M.insert idu uni{ordres=Pause} listeunis) bats
        Collecter -> let (Collecteur n max) = tu in if(n==max) then let coordraf = coordRaffinerie bats pu in 
                                                                    let listeunis= M.delete idu unis in 
                                                                    Environement joueurs carte (M.insert idu uni{ordres=Deplacer coordraf Collecter} listeunis) bats
                                                    else 
                                                        let carre = getCase cu carte in 
                                                            case carre of
                                                                Ressource r -> let valres = let res=n+r if (res>max) then res=max 
                                                                                            else res in
                                                                                let newcarte= M.delete cu carte 
                                                                                let listeunis= M.delete idu unis in             
                                                                                in Environement joueurs (M.insert cu (Ressource (r-(valres-n)))) (M.insert idu uni{typeu=Collecteur valres max} listeunis) bats
                                                                
                                                                _ -> let listeunis= M.delete idu unis in 
                                                                    let (Carte l h contenu)=carte
                                                                        (Coord x y )=cu in
                                                                    if(x>0 && x<l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord (x+1) y) Recherche} listeunis) bats
                                                                    else if(y>0 && y<l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord x (y+1)) Recherche} listeunis) bats
                                                                        else if(x==l) then Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord (x-1) y) Recherche} listeunis) bats
                                                                            else Environement joueurs carte (M.insert idu uni{ordres=Deplacer (Coord x (y-1)) Recherche} listeunis) bats


        PoserRaffinerie  -> let raf = trouver_raffinerie bats pu in let (Collecteur val max) = tu in 
                            modifier_credit idj env val
        


        Patrouiller c1 c2 -> let ennemis=trouver_ennemis unis cu pu in case ennemis of
                                                                    [] -> let listeunis= M.delete idu unis in 
                                                                        Environement joueurs carte (M.insert idu uni{ordres=Deplacer c1 (Patrouiller c1 c2)} listeunis) bats
                                                                    (Unite cen _ _ _ _ _):reste-> let listeunis= M.delete idu unis in 
                                                                        Environement joueurs carte (M.insert idu uni{ordres=Attaquer cen (Patrouiller c1 c2)} listeunis) bats

        Attaquer c ordrebase -> let ennemis = cherche_Case_Unite c unis in 
                                case ennemis of 
                                    []-> let listeunis= M.delete idu unis in 
                                        Environement joueurs carte (M.insert idu uni{ordres=Pause} listeunis) bats
                                    (Unite cen pen _ iden pven _):reste -> if(pen != pu) then 
                                        if((pven-6)<=0) then let listeunis= M.delete iden unis in 
                                                            let newunis = (M.insert iden uni{pvu=0} listeunis) in
                                                            let newunisbis = M.delete idu newunis in
                                                            Environement joueurs carte (M.insert idu uni{ordres=Pause} listeunis) bats
                                        else 
                                            let listeunis= M.delete iden unis in 
                                            Environement joueurs carte (M.insert iden uni{pvu=pven-6} listeunis) bats
        
        Pause -> env