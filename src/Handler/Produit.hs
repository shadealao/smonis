module Handler.Produit where

import Import
import Handler.Shared

ajouterProduitAForm :: Maybe Produit -> AForm Handler Produit
ajouterProduitAForm produit = Produit
    <$> areq textField (widgetsettings (SomeMessage MsgFieldName) "nom" []) (produitNom <$> produit)
    <*> areq doubleField (widgetsettings ("Prix TTC") "prix" []) (produitPrix <$> produit)
    <*> aopt textareaField (widgetsettings ("Description") "description" []) (produitDescription <$> produit)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing


showListProduit :: Widget
showListProduit = do 
    res <- handlerToWidget $ runDB $ selectList [ProduitId >. (fromJust $ fromPathPiece "0")][Asc ProduitNom]
    [whamlet|
        $forall Entity recordId record <- res 
            <div id=#{toPathPiece recordId}>
                <a title=modifier href="@{ProduitR}?Produitid=#{toPathPiece recordId}">
                    <span>#{produitNom record} - #{produitPrix record}€ TTC
                    $maybe textdescription <- (produitDescription record) 
                        <p>#{textdescription}
    |]



getProduitR :: Handler Html
getProduitR = do 
    produitid <- lookupGetParam "Produitid"
    (widget0, enctype0) <- case produitid of
        Nothing -> generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColMd 1) (ColMd 4) (ColMd 1) (ColMd 6)) (ajouterProduitAForm Nothing)
        (Just val) -> do
            res <- runDB $ selectFirst [ProduitId ==. (fromJust $ fromPathPiece val)] []
            generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColMd 1) (ColMd 4) (ColMd 1) (ColMd 6)) (ajouterProduitAForm (Just $ entityVal $ fromJust res))
    
    defaultLayout $ do
        $(widgetFile "produit")

postProduitR :: Handler Html
postProduitR = do 
    produitid <- lookupPostParam "produitid"
    maybeCurrentUserId <- maybeAuthId
    currentUTCTime <- liftIO getCurrentTime


    ((res0, widget0),enctype0) <- case produitid of
        Nothing -> runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColMd 1) (ColMd 4) (ColMd 1) (ColMd 6)) (ajouterProduitAForm Nothing)
        (Just val) -> do
            res <- runDB $ selectFirst [ProduitId ==. (fromJust $ fromPathPiece val)] []
            runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColMd 1) (ColMd 4) (ColMd 1) (ColMd 6)) (ajouterProduitAForm (Just $ entityVal $ fromJust res))

    _ <- case (res0, produitid) of
        (FormSuccess addProduit, Nothing) -> do
            let addProduit' = addProduit { produitCreated = Just currentUTCTime
                            , produitCreatorId = maybeCurrentUserId
                            , produitUpdated = Just currentUTCTime
                            , produitLastUpdatorId = maybeCurrentUserId
                            }
            newid <- runDB $ insert addProduit'
            setMessage "Nouveaux produit "
            redirect (ProduitR :#: newid)
        (FormSuccess addProduit, Just val) -> do
            let addProduit' = addProduit {produitUpdated = Just currentUTCTime
                            , produitLastUpdatorId = maybeCurrentUserId
                            }
            runDB $ replace (fromJust $ fromPathPiece val) addProduit'
            setMessage "MAJ produit "
            redirect (ProduitR :#: val)
        _ -> do
            setMessage "Action échouée"
            redirect ProduitR 

    defaultLayout $ do
        $(widgetFile "produit")

putProduitR :: Handler Html
putProduitR = error "Not yet implemented: putProduitR"

deleteProduitR :: Handler Html
deleteProduitR = error "Not yet implemented: deleteProduitR"
