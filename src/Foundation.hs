{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import Network.Mail.Mime as Mail
import Text.Shakespeare.Text     (stext)

import Yesod.Form.I18n.French

import qualified Data.List as List
import Translate ()
-------------------------------------------------------------------------------------
-- | tools functions
--

containElemWithKey :: (Eq a) => a -> [(a, b)] -> Bool
containElemWithKey key = List.foldl (\acc (k, _) -> if k == key then True else acc) False

-------------------------------------------------------------------------------------
-- | data for the model
--
data Role = RoleUser | RoleEleve | RoleEnseignant | RoleResponsableCatalogue | RoleAdmin deriving (Eq,Ord,Enum,Bounded)
instance Show Role where
    show RoleAdmin = "Admin"
    show RoleResponsableCatalogue = "Responsable de catalogue"
    show RoleEnseignant = "enseignant"
    show RoleEleve = "eleve"
    show RoleUser = "User"

-------------------------------------------------------------------------------------
-- | practical functions for the model

-- | Is user active
isMaybeUserActive :: Maybe User -> Bool
isMaybeUserActive user = fromMaybe False (userActive <$> user)

-- | Is user admin
isMaybeUserAdmin :: Maybe User -> Bool
isMaybeUserAdmin user = (userRole <$> user) == Just (fromString $ show $ RoleAdmin)

-- | Is user  responsable de catalogue
isMaybeUserResponsableCatalogue :: Maybe User -> Bool
isMaybeUserResponsableCatalogue user = (userRole <$> user) == Just (fromString $ show $ RoleResponsableCatalogue)

-- | Is user enseignant
isMaybeUserEnseignant :: Maybe User -> Bool
isMaybeUserEnseignant user = (userRole <$> user) == Just (fromString $ show $ RoleEnseignant)

-- | Is user eleve
isMaybeUserEleve :: Maybe User -> Bool
isMaybeUserEleve user = (userRole <$> user) == Just (fromString $ show $ RoleEleve)


-------------------------------------------------------------------------------------

-- | Is current user active and admin
isMaybeAuthPairUserActiveAdmin :: Maybe (UserId, User) -> Bool
isMaybeAuthPairUserActiveAdmin (Just (_, user))= isMaybeUserActive (Just user) && isMaybeUserAdmin (Just user)
isMaybeAuthPairUserActiveAdmin _ = False

-- | Is current user active and responsable de catalogue
isMaybeAuthPairUserActiveResponsableCatalogue :: Maybe (UserId, User) -> Bool
isMaybeAuthPairUserActiveResponsableCatalogue (Just (_, user))= isMaybeUserActive (Just user) && isMaybeUserResponsableCatalogue (Just user)
isMaybeAuthPairUserActiveResponsableCatalogue _ = False

-- | Is current user active and expert
isMaybeAuthPairUserActiveEnseignant :: Maybe (UserId, User) -> Bool
isMaybeAuthPairUserActiveEnseignant (Just (_, user))= isMaybeUserActive (Just user) && isMaybeUserEnseignant (Just user)
isMaybeAuthPairUserActiveEnseignant _ = False

-- | Is current user active and contributeur
isMaybeAuthPairUserActiveEleve :: Maybe (UserId, User) -> Bool
isMaybeAuthPairUserActiveEleve (Just (_, user))= isMaybeUserActive (Just user) && isMaybeUserEleve (Just user)
isMaybeAuthPairUserActiveEleve _ = False


-- | Is current user active and contributeur or responsable de catalogue or admin
isMaybeAuthPairUserActiveRA :: Maybe (UserId, User) -> Bool
isMaybeAuthPairUserActiveRA (Just (_, user))= isMaybeUserActive (Just user) && (isMaybeUserResponsableCatalogue (Just user) || isMaybeUserAdmin (Just user))
isMaybeAuthPairUserActiveRA _ = False


-- | Is current user active
isMaybeAuthPairUserActive :: Maybe (UserId, User) -> Bool
isMaybeAuthPairUserActive (Just (_, user)) = isMaybeUserActive (Just user)
isMaybeAuthPairUserActive _ = False

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have

-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text -- Translation--- RenderMessage -> Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- ===========================LANGUE ======================================
mkMessage "App" "messages" "fr"



-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 30 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend app = Just <$> defaultClientSessionBackend
        30    -- timeout in minutes
        (appClientSessionKF $ appSettings app)
    --    "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (_, _) <- breadcrumbs -- (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
    
    
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Page principale"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
               

               
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Créneaux"
                    , menuItemRoute = CreneauxR
                    , menuItemAccessCallback = isMaybeAuthPairUserActiveAdmin muser
                    }
                ,NavbarRight $ MenuItem
                    { menuItemLabel = "Tarifs"
                    , menuItemRoute = TarifR
                    , menuItemAccessCallback = isMaybeAuthPairUserActiveEleve muser
                    }
                ,NavbarRight $ MenuItem
                    { menuItemLabel = "Réservation"
                    , menuItemRoute = CoursR
                    , menuItemAccessCallback = isMaybeAuthPairUserActiveEleve muser
                    }
                ,NavbarRight $ MenuItem
                    { menuItemLabel = "Réservation"
                    , menuItemRoute = CoursEnseignantR
                    , menuItemAccessCallback = isMaybeAuthPairUserActiveAdmin muser
                    }
                
                ,NavbarRight $ MenuItem
                    { menuItemLabel = "Produits"
                    , menuItemRoute = ProduitR
                    , menuItemAccessCallback = isMaybeAuthPairUserActiveAdmin muser
                    }
                
                ,NavbarRight $ MenuItem
                    { menuItemLabel = "Gestion utilisateurs"
                    , menuItemRoute = UserR
                    , menuItemAccessCallback = isMaybeAuthPairUserActiveAdmin muser
                    }
                ,NavbarRight $ MenuItem
                    { menuItemLabel = "Panier"
                    , menuItemRoute = PanierR
                    , menuItemAccessCallback = isMaybeAuthPairUserActiveEleve muser
                    }
                ,  NavbarRight $ MenuItem
                    { menuItemLabel = "Profil"
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Se connecter"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Se déconneter"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Langue"
                    , menuItemRoute = LangR
                    , menuItemAccessCallback = True
                    }
                ]
        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addScript $ StaticR js_moment_with_locales_min_js
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized LangR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function
    isAuthorized ProfileR _ = isAuthenticated

    -- the user route requires that the user is authenticated and admin, so we
    -- delegate to that function
    isAuthorized StoreSessionR _ = isAuthenticated
    isAuthorized UserR _ = isAuthenticatedActiveAdmin
    isAuthorized CoursR _ = isAuthenticatedActiveEleve
    isAuthorized TarifR _ = isAuthenticatedActiveEleve
    isAuthorized PanierR _ = isAuthenticatedActiveEleve
    isAuthorized CreneauxR _ = isAuthenticatedActiveAdmin
    isAuthorized CoursEnseignantR _ = isAuthenticatedActiveAdmin
    isAuthorized ProduitR _ = isAuthenticatedActiveAdmin
    --isAuthorized HomeR _ = isAuthenticated
    isAuthorized (EditUserR _) _ = isAuthenticatedActiveAdmin
    isAuthorized CreateUserR _ = isAuthenticatedActiveAdmin

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR = return ("Page principale", Nothing)
    breadcrumb (AuthR _) = return ("Se connecter", Just HomeR)
    
    --breadcrumb DashboardStatR = return ("Graphs",  Just HomeR)
    breadcrumb  _ = return ("Page principale", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- insertBy $ User (credsIdent creds) (credsIdent creds) (Just "{}") Nothing Nothing False False
            (fromString (show RoleUser)) Nothing Nothing Nothing Nothing
        return $ Authenticated $
            case x of
                Left (Entity userid _) -> userid -- newly added user
                Right userid -> userid -- existing user

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authEmail]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

isAuthenticatedActive :: Handler AuthResult
isAuthenticatedActive = do
    muser <- maybeAuthPair
    localIsAuthenticated <- isAuthenticated
    if (localIsAuthenticated == Authorized)
        then if (isMaybeUserActive (snd <$> muser))
                 then return Authorized
                 else return (Unauthorized "You must have an active account to access this page")
        else return localIsAuthenticated

isAuthenticatedActiveEleve :: Handler AuthResult
isAuthenticatedActiveEleve = do
    muser <- maybeAuthPair
    localIsAuthenticated <- isAuthenticated
    if (localIsAuthenticated == Authorized)
        then if (isMaybeUserEleve (snd <$> muser))
                 then return Authorized
                 else return (Unauthorized "You must have an active account and a student to access this page")
        else return localIsAuthenticated

isAuthenticatedActiveAdmin :: Handler AuthResult
isAuthenticatedActiveAdmin = do
    muser <- maybeAuthPair
    localIsAuthenticatedActive <- isAuthenticatedActive
    if (localIsAuthenticatedActive == Authorized)
        then if (isMaybeUserAdmin (snd <$> muser))
                then return Authorized
                else return (Unauthorized "You must be admin to access this page")
        else return localIsAuthenticatedActive

instance YesodAuthPersist App

-- | For Login Form specialized MBJ: does not work => postponed task A:1of2
-- data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }

-- Here's all of the email-specific code
instance YesodAuthEmail App where
    type AuthEmailId App = UserId


    afterPasswordRoute _ = HomeR

    addUnverified email verkey =
        liftHandler $ runDB $ insert $ User email email (Just "{}") Nothing (Just verkey) False True
             (fromString (show RoleUser)) Nothing Nothing Nothing Nothing

    sendVerifyEmail email _ verurl = do
        -- Print out to the console the verification email, for easier
        -- debugging.
        liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" ++ verurl
        -- Send email.
        liftIO $ Mail.renderSendMail $ Mail.simpleMail'
            (Mail.Address Nothing email) -- To address, Nothing can be replaced with a name
            (Mail.Address (Just "Auto-école Smoni") "alao.a.s@gmail.com") -- From address
            "Verify your email address" -- Subject
            [stext|
                        Please confirm your email address by clicking on the link below.
                        #{verurl}
                        Thank you
                    |] -- Body of the email
    
    getVerifyKey = liftHandler . runDB . fmap (join . fmap userVerkey) . get

    setVerifyKey uid key = liftHandler $ runDB $ update uid [UserVerkey =. Just key]

    verifyAccount uid = liftHandler $ runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True, UserVerkey =. Nothing]
                return $ Just uid

    getPassword = liftHandler . runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = liftHandler . runDB $ update uid [UserPassword =. Just pass]

    getEmailCreds email = liftHandler $ runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = email
                }
    getEmail = liftHandler . runDB . fmap (fmap userEmail) . get


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ []        = frenchFormMessage -- Default to Swedish
    renderMessage _ ("fr":_) = frenchFormMessage
    renderMessage _ ("en":_) = defaultFormMessage -- English
    renderMessage m (_   :ls) = renderMessage m ls
    --renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- 
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirect HomeR

getLangR :: Handler Html
getLangR = defaultLayout
    [whamlet|
        <div .col-lg-6.col-lg-offset-3>
            <h3 .text-center>_{MsgChangerdelangue}
            <article .jumbotron .col-lg-12>
                <form action=@{LangR} method=post>
                    <select name=lang>
                        <option value=en>English
                        <option value=fr>Français
                    <input type=submit value=_{MsgChanger}>
    |]