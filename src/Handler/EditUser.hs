{-# OPTIONS_GHC -Wno-type-defaults #-}


module Handler.EditUser where

import Import

import Handler.Shared

editUserAForm :: Maybe User -> AForm Handler User
editUserAForm muser = User
    <$> areq textField (widgetsettings (SomeMessage MsgFieldName) "nom" []) (userName <$> muser)
    <*> areq emailField (widgetsettings (SomeMessage MsgEmail) "email" []) (userEmail <$> muser)
    <*> aopt textareaField (widgetsettings (SomeMessage MsgSetting) "setting" []) (userSettings <$> muser)
    <*> pure (maybe (Just "":: Maybe Text) userPassword muser)
    <*> aopt textField (widgetsettings (SomeMessage MsgVerificationKey) "verificationkey" []) (userVerkey <$> muser)
    <*> areq checkBoxField (widgetsettings (SomeMessage MsgVerifie) "verifie" [("class", "form-check form-check-inline"), ("style", "margin: 0px 6px;")]) (userVerified <$> muser)
    <*> areq boolField (widgetsettings (SomeMessage MsgActive) "active" [("class", "form-check form-check-inline"), ("style", "margin: 0px 6px;")]) (userActive <$> muser)
    <*> areq ((alignedRadioField . optionsPairs) roles) (widgetsettings (SomeMessage MsgRole) "userRole" [("class", "form-check form-check-inline"), ("style", "margin: 0px 6px;")]) (userRole <$> muser)
    <*> pure (join (userCreated <$> muser))
    <*> pure (join (userCreatorId <$> muser))
    <*> pure (join (userUpdated <$> muser))
    <*> pure (join (userLastUpdatorId <$> muser))
  where
    roles :: [(Text, Text)]
    roles = map (\x ->(fromString $ show x, fromString $ show x)) [(RoleUser)..]

editUserForm :: Maybe User -> Html -> MForm Handler (FormResult User, Widget)
editUserForm muser = renderDivsWithAttrs [
                                            [("class" :: Text, "field-text" :: Text)]
                                            ,[("class" :: Text, "field-email" :: Text)]
                                            ,[("class" :: Text, "field-textarea" :: Text)]
                                            ,[("class" :: Text, "field-text field-verkey" :: Text)]
                                            ,[("class" :: Text, "field-checkbox field-verified" :: Text)]
                                            ,[("class" :: Text, "field-bool field-active" :: Text)]
                                            ,[("class" :: Text, "field-aligned-radio field-role" :: Text)]
                                          ] $ editUserAForm $ muser

getEditUserR :: UserId -> Handler Html
getEditUserR uid = do
    inUser <- runDB $ get404 uid
    ((result, widget), enctype) <- runFormGet $ editUserForm $ Just inUser

    case result of
        FormSuccess outputUser -> do
            maybeCurrentUserId <- maybeAuthId
            currentUTCTime <- liftIO getCurrentTime
            let updates = if (maybeCurrentUserId == Nothing)
                then [ UserName =. userName outputUser
                     , UserEmail =. userEmail outputUser
                     , UserSettings =. userSettings outputUser
                     , UserVerkey =. userVerkey outputUser
                     , UserVerified =. userVerified outputUser
                     , UserActive =. userActive outputUser
                     , UserRole =. userRole outputUser
                     , UserUpdated =. Just currentUTCTime
                     ]
                else [ UserName =. userName outputUser
                     , UserEmail =. userEmail outputUser
                     , UserSettings =. userSettings outputUser
                     , UserVerkey =. userVerkey outputUser
                     , UserVerified =. userVerified outputUser
                     , UserActive =. userActive outputUser
                     , UserRole =. userRole outputUser
                     , UserUpdated =. Just currentUTCTime
                     , UserLastUpdatorId =. maybeCurrentUserId
                     ]
            -- updatedUser <- runDB $ updateGet uid updates
            -- defaultLayout [whamlet|<p>Updated #{show updatedUser}|]
            runDB $ update uid updates

            _ <- case (userRole outputUser) of 
                "enseignant" -> putEnseignant uid
                "eleve" -> putEleve uid
                _ ->  sendResponseStatus status200 ("UPDATED User " ++ (toPathPiece uid))
        
            redirect (UserR :#: uid)
        FormFailure msg -> defaultLayout [whamlet|<p>Form failed #{show msg}|]
        FormMissing -> defaultLayout $ do
            let editUserFormId = editUserIds
            setTitle . toHtml $ ("Edit User Page" :: Text)
            $(widgetFile "edit-user")

editUserIds :: (Text)
editUserIds = ("js-editUserForm")

------------------------------------------------------------------

putEleve :: UserId -> Handler Value
putEleve uid = do
    maybeCurrentUserId <- maybeAuthId
    currentUTCTime <- liftIO getCurrentTime
    _ <- runDB $  insert $ Eleve uid "nom" 0 (Just currentUTCTime) maybeCurrentUserId (Just currentUTCTime) maybeCurrentUserId
    -- sendResponseStatus status200 ("UPDATED " ++ (show updatedUser))
    sendResponseStatus status200 ("UPDATED User " ++ (toPathPiece uid))

putEnseignant :: UserId -> Handler Value
putEnseignant uid = do
    maybeCurrentUserId <- maybeAuthId
    currentUTCTime <- liftIO getCurrentTime
    _ <- runDB $  insert $ Enseignant uid "nom" (Just currentUTCTime) maybeCurrentUserId (Just currentUTCTime) maybeCurrentUserId
    -- sendResponseStatus status200 ("UPDATED " ++ (show updatedUser))
    sendResponseStatus status200 ("UPDATED User " ++ (toPathPiece uid))

putEditUserJson :: UserId -> Handler Value
putEditUserJson uid = do
    updatedUser <- requireCheckJsonBody :: Handler User
    runDB $ replace uid updatedUser
    -- sendResponseStatus status200 ("UPDATED " ++ (show updatedUser))
    sendResponseStatus status200 ("UPDATED User " ++ (toPathPiece uid))

------------------------------------------------------------------

putEditUserR :: UserId -> Handler TypedContent
putEditUserR uid = selectRep $ do
    provideRep $ (putEditUserJson uid)

------------------------------------------------------------------

deleteEditUserR :: UserId -> Handler TypedContent
deleteEditUserR uid = do
    runDB $ delete uid
    sendResponseStatus status200 ("DELETED" :: Text)


