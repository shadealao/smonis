module Handler.Creneaux where

import Import
import Handler.Shared
--import Data.Text.ICU.Calendar as DT

--func


ajouterCreneauxAForm :: Maybe Creneaux -> AForm Handler Creneaux
ajouterCreneauxAForm creneaux = Creneaux
    <$> areq dayField (widgetsettings (SomeMessage MsgFieldStartDate) "date_creneaux" []) Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*  areq timeField (widgetsettings ("Heure début") "heure_creneaux_debut" []) Nothing
    <*  areq timeField (widgetsettings ("Heure fin") "heure_creneaux_fin" []) Nothing
    <*> areq boolField (widgetsettings ("active") "accesrevers" [("class", "form-check form-check-inline"), ("style", "margin: 0px 6px;")]) (creneauxActive <$> creneaux)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing


showListCreneaux :: Widget
showListCreneaux = do 
    res <- handlerToWidget $ runDB $ selectList [CreneauxId >. (fromJust $ fromPathPiece "0")][Asc CreneauxDateCreneaux, Asc CreneauxHeureCreneauxDebut]
    [whamlet|
        $forall Entity recordId record <- res 
            <div id=#{toPathPiece recordId}>
                <span>#{dayFormat2 $ Just $ creneauxDateCreneaux record} (#{maybeText $ creneauxHeureCreneauxDebut record} - #{maybeText $ creneauxHeureCreneauxFin record})
                $if ((creneauxActive record) == True)
                    <span .text-success> Libre
                $else
                    <span .text-danger> Réservé
    |]


getCreneauxR :: Handler Html
getCreneauxR = do 
    creneauxid <- lookupGetParam "creneauxid"
    (widget0, enctype0) <- case creneauxid of
        Nothing -> generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColMd 1) (ColMd 4) (ColMd 1) (ColMd 6)) (ajouterCreneauxAForm Nothing)
        (Just val) -> do
            res <- runDB $ selectFirst [CreneauxId ==. (fromJust $ fromPathPiece val)] []
            generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColMd 1) (ColMd 4) (ColMd 1) (ColMd 6)) (ajouterCreneauxAForm (Just $ entityVal $ fromJust res))
    
    defaultLayout $ do
        $(widgetFile "creneaux")

postCreneauxR :: Handler Html
postCreneauxR = do
    creneauxid <- lookupPostParam "creneauxid"
    heure_creneaux_debut <- lookupPostParam "heure_creneaux_debut"
    heure_creneaux_fin <- lookupPostParam "heure_creneaux_fin"
    maybeCurrentUserId <- maybeAuthId
    currentUTCTime <- liftIO getCurrentTime


    ((res0, widget0),enctype0) <- case creneauxid of
        Nothing -> runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColMd 1) (ColMd 4) (ColMd 1) (ColMd 6)) (ajouterCreneauxAForm Nothing)
        (Just val) -> do
            res <- runDB $ selectFirst [CreneauxId ==. (fromJust $ fromPathPiece val)] []
            runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColMd 1) (ColMd 4) (ColMd 1) (ColMd 6)) (ajouterCreneauxAForm (Just $ entityVal $ fromJust res))

    _ <- case (res0, creneauxid) of
        (FormSuccess addCreneaux, Nothing) -> do
            let addCreneaux' = addCreneaux { creneauxCreated = Just currentUTCTime
                            , creneauxCreatorId = maybeCurrentUserId
                            , creneauxUpdated = Just currentUTCTime
                            , creneauxLastUpdatorId = maybeCurrentUserId
                            , creneauxHeureCreneauxDebut = heure_creneaux_debut
                            , creneauxHeureCreneauxFin = heure_creneaux_fin
                            }
            newid <- runDB $ insert addCreneaux'
            setMessage "Nouveaux creneaux "
            redirect (CreneauxR :#: newid)
        (FormSuccess addCreneaux, Just val) -> do
            let addCreneaux' = addCreneaux {creneauxUpdated = Just currentUTCTime
                            , creneauxLastUpdatorId = maybeCurrentUserId
                            }
            runDB $ replace (fromJust $ fromPathPiece val) addCreneaux'
            setMessage "MAJ creneaux "
            redirect (CreneauxR :#: val)
        _ -> do
            setMessage "Action échouée"
            redirect CreneauxR 

    defaultLayout $ do
        $(widgetFile "creneaux")

putCreneauxR :: Handler Html
putCreneauxR = error "Not yet implemented: putCreneauxR"

deleteCreneauxR :: Handler Html
deleteCreneauxR = error "Not yet implemented: deleteCreneauxR"
