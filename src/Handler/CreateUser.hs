{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.CreateUser where

import Import

import Handler.Shared

createUserAForm :: AForm Handler User
createUserAForm = User
    <$> areq textField (widgetsettings (SomeMessage MsgFieldName) "nom" []) Nothing
    <*> areq emailField (widgetsettings (SomeMessage MsgEmail) "email" []) Nothing
    <*> aopt textareaField (widgetsettings (SomeMessage MsgSetting) "setting" []) Nothing -- (Just "{}"::Textarea)
    <*> pure Nothing
    <*> aopt textField (widgetsettings (SomeMessage MsgVerificationKey) "verificationkey" []) Nothing
    <*> areq checkBoxField (widgetsettings (SomeMessage MsgVerifie) "verifie" [("class", "form-check form-check-inline"), ("style", "margin: 0px 6px;")]) (Just True)
    <*> areq boolField (widgetsettings (SomeMessage MsgActive) "active" [("class", "form-check form-check-inline"), ("style", "margin: 0px 6px;")]) (Just False)
    <*> areq ((alignedRadioField . optionsPairs) roles) (widgetsettings (SomeMessage MsgRole) "userRole" [("class", "form-check form-check-inline"), ("style", "margin: 0px 6px;")]) (Just $ pack $ show RoleUser)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
  where
    roles :: [(Text, Text)]
    roles = map (\x ->(pack $ show x, pack $ show x)) [(RoleUser)..]

createUserForm :: Html -> MForm Handler (FormResult User, Widget)
createUserForm = renderDivsWithAttrs [
                                            [("class" :: Text, "field-text" :: Text)]
                                            ,[("class" :: Text, "field-email" :: Text)]
                                            ,[("class" :: Text, "field-textarea" :: Text)]
                                            ,[("class" :: Text, "field-text field-verkey" :: Text)]
                                            ,[("class" :: Text, "field-checkbox field-verified" :: Text)]
                                            ,[("class" :: Text, "field-bool field-active" :: Text)]
                                            ,[("class" :: Text, "field-aligned-radio field-role" :: Text)]
                                          ] createUserAForm

getCreateUserR :: Handler Html
getCreateUserR = do
    ((result, widget), enctype) <- runFormGet createUserForm
    case result of
        FormSuccess user -> do
            maybeCurrentUserId <- maybeAuthId
            currentUTCTime <- liftIO getCurrentTime
            let user' = user { userCreated = Just currentUTCTime
                             , userCreatorId = maybeCurrentUserId
                             , userUpdated = Just currentUTCTime
                             , userLastUpdatorId = maybeCurrentUserId
                             }

            insertedUser <- runDB $ insertEntity user'
            -- send verificationEmail
            {-
            y <- getYesod
            verKey <- liftIO $ randomKey y
            user <- entityVal insertedUser
            uid <- entityKey insertedUser
            email <- userEmail $ entityVal insertedUser
            runDB $ update user [UserVerkey =. Just verKey] --  setVerifyKey uid verKey
            render <- getUrlRender
--            tp <- getRouteToParent AuthR
            let verUrl = render $ AuthR $ verifyR (toPathPiece uid) verKey (isJust Nothing)
            sendVerifyEmail email verKey verUrl -- BUGBUG
            -}
            -- defaultLayout [whamlet|<p>Created #{show (entityVal insertedUser)}|]
            redirect (UserR :#: entityKey insertedUser)
        FormFailure msg -> defaultLayout [whamlet|<p>Form failed #{show msg}|]
        FormMissing -> defaultLayout $ do
            let createUserFormId = createUserIds
            setTitle . toHtml $ ("Create User Page" :: Text)
            $(widgetFile "create-user")

createUserIds :: Text
createUserIds = "js-createUserForm"

