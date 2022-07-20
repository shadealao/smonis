{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.User where

import Import

import Handler.Shared

--import Text.Julius (juliusFile)

--import Database.Persist.Sql (rawSql, Single, unSingle)

getUserR :: Handler Html
getUserR = do
--    allUsers <- runDB $ getAllUsers
    allUsers <- runDB getAllUsersWithCreatorAndUpdator
    maybeCurrentUserId <- maybeAuthId

    defaultLayout $ do
        currentAuthUser <- liftHandler getAuthUser
        let (userListId, authUser) = (userIds, (toJSON currentAuthUser))
        setTitle . toHtml $ ("Users Page" :: Text)
        toWidget $(juliusFile "templates/menu-display-columns.julius")
        $(widgetFile "user")

userIds :: (Text)
userIds = ("js-userList")

--getAllUsers :: DB [Entity User]
--getAllUsers = selectList [] [Asc UserId]

getAllUsersWithCreatorAndUpdator :: MonadIO m => ReaderT SqlBackend m [(Entity User, Maybe (Single Text), Maybe (Single Text))]
getAllUsersWithCreatorAndUpdator = rawSql
        "SELECT ??, user1.email, user2.email \
        \ FROM public.user \
        \ LEFT OUTER JOIN public.user AS user1 \
        \ ON public.user.creator_id = user1.id \
        \ LEFT OUTER JOIN public.user AS user2 \
        \ ON public.user.last_updator_id = user2.id \
        \ ORDER BY public.user.id"
        []  

