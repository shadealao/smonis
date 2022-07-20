module Handler.Cours where

import Import
import Handler.Shared




showListCoursFromEleve :: EleveId -> Widget
showListCoursFromEleve eleveid = do 
    res <- handlerToWidget $ runDB $ selectList [CoursIdEleve ==. eleveid][]
    [whamlet|
        $forall Entity _ record <- res 
            <div>
                <span>^{showSpecifiqueCreneaux (coursIdCreneaux record)}
                $maybe _ <- (coursIdEnseignant record)
                    <span .text-success>validé
                $nothing
                    <span .text-warning>En cours de validation
                
    |]


getCoursR :: Handler Html
getCoursR = do
    maybeCurrentUserId <- maybeAuthId
    eleveid <- case maybeCurrentUserId of 
        (Just userid) -> runDB $ selectFirst [EleveIdUser ==. userid ] []
        Nothing -> return Nothing

    defaultLayout $ do
        $(widgetFile "cours")

postCoursR :: Handler Html
postCoursR = do
    maybeCurrentUserId <- maybeAuthId
    eleveid <- case maybeCurrentUserId of 
        (Just userid) -> runDB $ selectFirst [EleveIdUser ==. userid ] []
        Nothing -> return Nothing
    defaultLayout $ do
        $(widgetFile "cours")

putCoursR :: Handler TypedContent
putCoursR = do
    -- table <- lookupGetParam "table"
    maybeCurrentUserId <- maybeAuthId
    selectRep $ provideRep $ addCoursFromEleve (fromJust maybeCurrentUserId)
    {-
    case table of 
        "eleve" -> selectRep $ provideRep $ addCoursFromEleve (fromJust maybeCurrentUserId)
        "enseignant" -> error "Not yet implemented: add cours from enseignant "
        _ -> error "Not yet implemented: add cours putr  "
    -}

deleteCoursR :: Handler Html
deleteCoursR = error "Not yet implemented: deleteCoursR"


addCoursFromEleve :: UserId -> Handler Value
addCoursFromEleve userid = do 
    idcreneraux <- lookupGetParam "idcreneraux"
    maybeCurrentUserId <- maybeAuthId
    currentUTCTime <- liftIO getCurrentTime
    case idcreneraux of
        (Just value) -> do 
            crenerauxid <- runDB $ selectFirst [CreneauxId ==. (fromJust $ fromPathPiece value) ] []
            eleveid <- runDB $ selectFirst [EleveIdUser ==. userid ] []
            res <- runDB $ insert $ Cours (entityKey $ fromJust crenerauxid) Nothing (entityKey $ fromJust eleveid) False (Just currentUTCTime) maybeCurrentUserId (Just currentUTCTime) maybeCurrentUserId
            runDB $ updateWhere [CreneauxId ==. (fromJust $ fromPathPiece value)] [ CreneauxActive =. False]
            runDB $ updateWhere [EleveId ==. (entityKey $ fromJust eleveid)] [ EleveHeureRestantes =. ((eleveHeureRestantes $ entityVal $ fromJust eleveid) -1 )]
            return $ object ["newCours" .= res]
        _ -> sendResponseStatus status400 ("Missing mandatory query parameter" :: Text)


