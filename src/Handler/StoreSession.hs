{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.StoreSession where

import Import

import Data.Aeson (encode, decodeStrict)

---------------------------------------------------------------- 

getStoreSessionJson :: Handler Value 
getStoreSessionJson = do 
    maybeKey <- lookupGetParam "key"
    case maybeKey of Nothing -> do
                                        sendResponseStatus status400 ("Missing mandatory query parameter" :: Text)
                     Just theKey -> do 
                                        readEncodedData <- lookupSessionBS theKey
                                        return $ fromMaybe (fromJust $ decodeStrict "null") (decodeStrict (fromMaybe "null" readEncodedData) :: Maybe Value)

getStoreSessionR :: Handler TypedContent
getStoreSessionR = selectRep $ do 
    provideRep $ getStoreSessionJson 

---------------------------------------------------------------- 

putStoreSessionJson :: Handler Value 
putStoreSessionJson = do 
    maybeKey <- lookupGetParam "key"
    case maybeKey of Nothing -> do 
                                        sendResponseStatus status400 ("Missing mandatory query parameter" :: Text)
                     Just theKey -> do
                                        newData <- requireCheckJsonBody :: Handler Value
                                        let encodedData = (encode newData)
                                        setSessionBS theKey (toStrict encodedData)
                                        return $ newData

putStoreSessionR :: Handler TypedContent 
putStoreSessionR = selectRep $ do 
    provideRep $ putStoreSessionJson 
 
---------------------------------------------------------------- 

deleteStoreSessionJson :: Handler Value
deleteStoreSessionJson = do
    maybeKey <- lookupGetParam "key"
    case maybeKey of Nothing -> do 
                                        sendResponseStatus status400 ("Missing mandatory query parameter" :: Text)
                     Just theKey -> do                            
                                        deleteSession theKey
                                        sendResponseStatus status200 (("DELETED " ++  theKey):: Text)

deleteStoreSessionR :: Handler TypedContent
deleteStoreSessionR = selectRep $ do
    provideRep $ deleteStoreSessionJson

