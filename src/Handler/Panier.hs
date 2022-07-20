module Handler.Panier where

import Import

getPanierR :: Handler Html
getPanierR = do
    defaultLayout $ do
        toWidget $(juliusFile "templates/store-session.julius")
        $(widgetFile "panier")

postPanierR :: Handler Html
postPanierR = error "Not yet implemented: postPanierR"

putPanierR :: Handler Html
putPanierR = error "Not yet implemented: putPanierR"

deletePanierR :: Handler Html
deletePanierR = error "Not yet implemented: deletePanierR"
