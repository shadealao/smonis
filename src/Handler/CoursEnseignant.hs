module Handler.CoursEnseignant where

import Import
import Handler.Shared

getCoursPasseEffectue :: MonadIO m => ReaderT SqlBackend m [(
                        Entity Cours
                        , Maybe (Entity Creneaux)
                        , Maybe (Entity Eleve)
                        , Maybe (Entity Enseignant)
                        )]
getCoursPasseEffectue = rawSql
                        "SELECT ??, ??, ??, ?? \
                        \FROM cours \
                        \INNER JOIN creneaux ON creneaux.id = cours.id_creneaux \
                        \INNER JOIN eleve ON eleve.id = cours.id_eleve \
                        \INNER JOIN enseignant ON enseignant.id = cours.id_enseignant \
                        \WHERE cours.id_enseignant IS NOT NULL \
                        \AND cours.effectue = 'true' \
                        \AND (creneaux.date_creneaux <= CURRENT_DATE) "
                        []

getCoursPasseNonEffectue :: MonadIO m => ReaderT SqlBackend m [(
                        Entity Cours
                        , Maybe (Entity Creneaux)
                        , Maybe (Entity Eleve)
                        , Maybe (Entity Enseignant)
                        )]
getCoursPasseNonEffectue = rawSql
                        "SELECT ??, ??, ??, ?? \
                        \FROM cours \
                        \INNER JOIN creneaux ON creneaux.id = cours.id_creneaux \
                        \INNER JOIN eleve ON eleve.id = cours.id_eleve \
                        \INNER JOIN enseignant ON enseignant.id = cours.id_enseignant \
                        \WHERE cours.id_enseignant IS NOT NULL \
                        \AND cours.effectue = 'false' \
                        \AND (creneaux.date_creneaux <= CURRENT_DATE) "
                        []


getCoursNonPasseNonEffectue :: MonadIO m => ReaderT SqlBackend m [(
                        Entity Cours
                        , Maybe (Entity Creneaux)
                        , Maybe (Entity Eleve)
                        , Maybe (Entity Enseignant)
                        )]
getCoursNonPasseNonEffectue = rawSql
                        "SELECT ??, ??, ??, ?? \
                        \FROM cours \
                        \INNER JOIN creneaux ON creneaux.id = cours.id_creneaux \
                        \INNER JOIN eleve ON eleve.id = cours.id_eleve \
                        \INNER JOIN enseignant ON enseignant.id = cours.id_enseignant \
                        \WHERE cours.id_enseignant IS NOT NULL \
                        \AND cours.effectue = 'false' \
                        \AND (creneaux.date_creneaux > CURRENT_DATE) "
                        []




showCoursNonAttribues :: Widget
showCoursNonAttribues = do 
    res <- handlerToWidget $ runDB $ selectList [CoursIdEnseignant ==. Nothing][]
    [whamlet|
        $forall Entity recordId record <- res 
            <p>
                <a href="@{CoursEnseignantR}?idcours=#{toPathPiece recordId}"> ^{showSpecifiqueCreneaux $ coursIdCreneaux record}
    |]

showCoursPasseEffectue :: Widget
showCoursPasseEffectue = do 
    res <- handlerToWidget $ runDB $ getCoursPasseEffectue
    [whamlet|
        $forall(Entity recordcoursId _, (Just recordcreneraux ), (Just recordeleveve), (Just recordenseignant))  <- res 
            <p>
                <span>^{showSpecifiqueCreneaux $ entityKey recordcreneraux}
                <span>^{showSpecifiqueEleve $ entityKey recordeleveve}
                <span>^{showSpecifiqueEnseignant $ entityKey recordenseignant}
                <span>^{showSpecifiqueCours recordcoursId}
    |]

showCoursPasseNonEffectue :: Widget
showCoursPasseNonEffectue = do 
    res <- handlerToWidget $ runDB $ getCoursPasseNonEffectue
    [whamlet|
        $forall(Entity recordcoursId _, (Just recordcreneraux ), (Just recordeleveve), (Just recordenseignant))  <- res 
            <p>
                <button type=button .courseffectue value=#{toPathPiece recordcoursId}>
                    <span>^{showSpecifiqueCreneaux $ entityKey recordcreneraux}
                    <span>^{showSpecifiqueEleve $ entityKey recordeleveve}
                    <span>^{showSpecifiqueEnseignant $ entityKey recordenseignant}
                    <span>^{showSpecifiqueCours recordcoursId}
    |]

showCoursNonPasseNonEffectue :: Widget
showCoursNonPasseNonEffectue = do 
    res <- handlerToWidget $ runDB $ getCoursNonPasseNonEffectue
    [whamlet|
        $forall(Entity recordcoursId _, (Just recordcreneraux ), (Just recordeleveve), (Just recordenseignant))  <- res 
            <p>
                <a href="@{CoursEnseignantR}?idcours=#{toPathPiece recordcoursId}" title=Modifier>
                    <span>^{showSpecifiqueCreneaux $ entityKey recordcreneraux}
                    <span>^{showSpecifiqueEleve $ entityKey recordeleveve}
                    <span>^{showSpecifiqueEnseignant $ entityKey recordenseignant}
                    <span>^{showSpecifiqueCours recordcoursId}
    |]

getCoursEnseignantR :: Handler Html
getCoursEnseignantR = do
    idcours' <- lookupGetParam "idcours"
    enseignantsdisponibles <- runDB $ selectList[EnseignantId >. (fromJust $ fromPathPiece "0")][]
    idcours <- case idcours' of 
        (Just val) -> runDB $ selectFirst [CoursId ==. (fromJust $ fromPathPiece val)] []
        _ -> return Nothing
    defaultLayout $ do
        $(widgetFile "cours_enseignant")

postCoursEnseignantR :: Handler Html
postCoursEnseignantR = do
    idcours' <- lookupPostParam "idcours"
    idcours <- runDB $ selectFirst [CoursId ==. (fromJust $ fromPathPiece $ fromJust idcours')][]
    enseignantsdisponibles' <- lookupPostParam "enseignantsdisponibles"
    enseignantsdisponibles <- runDB $ selectList[EnseignantId ==. (fromJust $ fromPathPiece $ fromJust enseignantsdisponibles')][]
    _ <- case (idcours, enseignantsdisponibles) of 
        (Just (Entity _ _),[Entity _ _]) -> do 
            runDB $ updateWhere [CoursId ==. (fromJust $ fromPathPiece $ fromJust idcours')] [ CoursIdEnseignant =. (fromPathPiece $ fromJust enseignantsdisponibles')]
            redirect CoursEnseignantR
        _ -> do
            redirect CoursEnseignantR
    defaultLayout $ do
        $(widgetFile "cours_enseignant")

putCoursEnseignantR :: Handler TypedContent
putCoursEnseignantR = do
    idcours' <- lookupGetParam "idcours"
    idcours <- runDB $ selectFirst [CoursId ==. (fromJust $ fromPathPiece $ fromJust idcours')][]
    case idcours of 
        (Just (Entity _ _)) -> do 
            _ <- runDB $ updateWhere [CoursId ==. (fromJust $ fromPathPiece $ fromJust idcours')] [ CoursEffectue =. True]
            sendResponseStatus status200 ("OK " :: Text)
        _ -> sendResponseStatus status400 ("PbE" :: Text)

deleteCoursEnseignantR :: Handler Html
deleteCoursEnseignantR = error "Not yet implemented: deleteCoursEnseignantR"
