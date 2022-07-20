module Handler.Tarif where

import Import

showListProduitEleve :: Widget
showListProduitEleve = do 
    res <- handlerToWidget $ runDB $ selectList [ProduitId >. (fromJust $ fromPathPiece "0")][Asc ProduitNom]
    [whamlet|
        $forall Entity recordId record <- res 
            <div id=#{toPathPiece recordId}>
                <span>#{produitNom record} - #{produitPrix record}€ TTC
                $maybe textdescription <- (produitDescription record) 
                    <p>#{textdescription}
                <p>
                    <button type=button .ajouterpanier .glyphicon .glyphicon-shopping-cart .text-info title="ajouter au panier" prix=#{produitPrix record} nom=#{produitNom record} value=#{toPathPiece recordId}>
    
    |]

getTarifR :: Handler Html
getTarifR = do 
    defaultLayout $ do
        toWidget $(juliusFile "templates/store-session.julius")
        $(widgetFile "tarif")

postTarifR :: Handler Html
postTarifR = error "Not yet implemented: postTarifR"

putTarifR :: Handler Html
putTarifR = error "Not yet implemented: putTarifR"

deleteTarifR :: Handler Html
deleteTarifR = error "Not yet implemented: deleteTarifR"
