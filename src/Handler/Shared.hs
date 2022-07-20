{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Shared handler functions.
module Handler.Shared where

import Import

import Data.Time.LocalTime (localDay, zonedTimeToLocalTime, getZonedTime)

import Data.Aeson (withObject)
import Text.Read
import qualified Data.Text as T
import Translate()
-------------------------------------------------------------------------------------
-- | practical data form JSON communication
--

data IdList = IdList { ids :: [Text] } deriving Show

idListIds :: IdList -> [Text]
idListIds (IdList l) = l

instance FromJSON IdList where
    parseJSON = withObject "idList" $ \o ->
        IdList <$> o .: "ids"

-------------------------------------------------------------------------------------
-- | practical functions for form construction
--

-- | Creates an input with @type="radio"@ for selecting one option.
alignedRadioField :: (Eq a, RenderMessage site FormMessage)
           => HandlerFor site (OptionList a)
           -> Field (HandlerFor site) a
alignedRadioField = selectFieldHelper
    (\theId _name _attrs inside -> [whamlet|
$newline never
<div ##{theId} .aligned-radio>^{inside}
|])
    (\theId name isSel -> [whamlet|
$newline never
<label for=#{theId}-none>
        <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
        _{MsgSelectNone}
|])
    (\theId name attrs value isSel text -> [whamlet|
$newline never
<label for=#{theId}-#{value}>
        <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
        \#{text}
|])

-- | Same as renderDivs but add the attrs to the div for each view
renderDivsWithAttrs :: Monad m => [[(Text, Text)]] -> FormRender m a
renderDivsWithAttrs attributes aform fragment = do
    (res, views') <- aFormToForm aform
    let views = do views' []
    -- prepare the proper list of tuples [(view, attrs)] extending the "class" attribute with required or optional
    let classToAddList = do map (\v -> if (fvRequired v) then "required" else "optional") views
    let extendedAttibutes = do zipWith addAttrCssClass classToAddList attributes
    -- zip for whamlet $forall
    let viewsAndAttrs = do zip views extendedAttibutes
    let widget = [whamlet|
$newline never
\#{fragment}
$forall (view, attrs) <- viewsAndAttrs
     <div *{attrs}>
        <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
|]
    return (res, widget)
  where
      -- add find or create the attibute for "class" and add c
      addAttrCssClass :: Text -> [(Text, Text)] -> [(Text, Text)]
      addAttrCssClass c as = if containElemWithKey "class" as then map (addCssClass c) as else ("class", c):as
      -- add c to values in ("class", values)
      addCssClass :: Text -> (Text, Text) -> (Text, Text)
      addCssClass c ("class", "") = ("class", c)
      addCssClass c ("class", val) = ("class", c ++ " " ++ val)
      addCssClass _ (key, val) = (key, val)


-------------------------------------------------------------------------------------
-- | practical functions for display
--

-- format a date to display it is an UTC date time
dateFormat :: Maybe UTCTime -> Text
dateFormat (Nothing) = pack ""
dateFormat (Just t) = pack (formatTime defaultTimeLocale dateTimeFormat t)
                where dateTimeFormat = "%Y-%m-%dT%T.000+00:00" -- '2019-08-26T14:58:54.000+00:00'

maybeText :: Maybe Text -> Text
maybeText Nothing = ""
maybeText (Just val) = val

{-
-- format a date to display it is an UTC date time
textToUTC :: Text -> Text -> UTCTime
textToUTC (Nothing) = pack ""
dateFormat (Just t) = pack (formatTime defaultTimeLocale dateTimeFormat t)
                where dateTimeFormat = "%Y-%m-%dT%T.000+00:00" -- '2019-08-26T14:58:54.000+00:00'
-}
-- format a day to display
dayFormat :: Maybe Day -> Text
dayFormat (Nothing) = pack ""
dayFormat (Just t) = pack (formatTime defaultTimeLocale dayTimeFormat t)
                where dayTimeFormat = "%Y-%m-%d" -- '2019-08-26'

-- format a day to display
dayFormat2 :: Maybe Day -> Text
dayFormat2 (Nothing) = pack "Inconnue"
dayFormat2 (Just t) = pack (formatTime defaultTimeLocale dayTimeFormat t)
                where dayTimeFormat = "%d-%m-%Y" -- '26-08-2019'

-- format a day to display yeay
dayFormatYear :: Maybe Day -> Text
dayFormatYear (Nothing) = pack ""
dayFormatYear (Just t) = pack (formatTime defaultTimeLocale dayTimeFormat t)
                where dayTimeFormat = "%Y" -- '26-08-2019'

-- format a day to display
dayFormatText :: Maybe Day -> Text
dayFormatText (Nothing) = pack ""
dayFormatText (Just t) = pack (formatTime defaultTimeLocale dayTimeFormat t)
                where dayTimeFormat = "%d %B %Y" -- '26 aout 2019'

yearToDecade :: [Text] -> Text
yearToDecade [_, _, c, _] = c++"0'"
yearToDecade _ = "0"

yMD :: [Text] -> (Integer, Int, Int)
yMD [] = (0, 0, 0)
yMD [x,y,z] = ((read e :: Integer) ,(read d :: Int),(read f :: Int))
    where
        e = unpack x
        d = unpack y
        f = unpack z
yMD _ = (0, 0, 0)

yMDAnnee :: [Text] -> Integer
yMDAnnee [] = 0
yMDAnnee [x,_,_] = read e :: Integer
    where
        e = unpack x
yMDAnnee _ = 0



yMDMois :: [Text] -> Int
yMDMois [] = 0
yMDMois [_,x,_] = (read e :: Int)
    where
        e = unpack x
yMDMois _ = 0

yMDJour :: [Text] -> Int
yMDJour [] = 0
yMDJour [_,_,x] = (read e :: Int)
    where
        e = unpack x
yMDJour _ = 0

textToDate :: Text -> Day
textToDate date1 =
    let date = T.splitOn "-" date1
    in
        let annee = yMDAnnee date
            mois = yMDMois date
            jour = yMDJour date
        in fromGregorian annee mois jour

maybeTextToDate :: Maybe Text -> Maybe Day
maybeTextToDate Nothing = Nothing
maybeTextToDate date1 =
    let date = T.splitOn "-" (fromJust date1)
    in
        let annee = yMDAnnee date
            mois = yMDMois date
            jour = yMDJour date
        in Just (fromGregorian annee mois jour)


yMD3 :: [Text] -> (Text, Text, Text)
yMD3 [] = ("", "", "")
yMD3 [x,y,z] = (read e :: Text ,read d :: Text,read f :: Text)
    where
        e = unpack x
        d = unpack y
        f = unpack z
yMD3 _ = ("", "", "")

--- chronologie date
chronologieDate :: Day -> Day -> Bool
chronologieDate x y
    | x <= y = True
    | otherwise = False

verifiedChronologieDate :: Day -> Day -> Day -> Day -> Bool
verifiedChronologieDate sd1 sd2 ed1 ed2 =
    let aa = chronologieDate sd1 sd2
        bb = chronologieDate sd2 ed1
        cc = chronologieDate ed1 ed2
    in aa && bb && cc

trueORfalse :: Maybe Text -> Bool
trueORfalse (Just "yes") = True
trueORfalse _ = False

-------------------------------------------------------------------------------------
-- | practical functions for date
--

-- | Get the current local date.
getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime

-------------------------------------------------------------------------------------
-- | practical functions for authentified user
--
getAuthUser :: Handler (Entity User)
getAuthUser = do
            maybeAuthUser <- maybeAuth
            let authUser = fromJust maybeAuthUser
            return authUser;

-------------------------------------------------------------------------------------
-- | practical functions for Css
--
itemActiveCssClass :: Bool -> String
itemActiveCssClass a = if a then "" else "item-not-active"





-- Widget settiings 
widgetsettings  :: SomeMessage master -> Text -> [(Text, Text)] -> FieldSettings master
widgetsettings label ident [] = FieldSettings label Nothing (Just ident) (Just ident) [("class", "form-control")]
widgetsettings label ident listclass = FieldSettings label Nothing (Just ident) (Just ident) listclass





















showListCreneauxActive :: Widget
showListCreneauxActive = do 
    res <- handlerToWidget $ runDB $ selectList [CreneauxActive ==. True][Asc CreneauxDateCreneaux, Asc CreneauxHeureCreneauxDebut]
    [whamlet|
        $forall Entity recordId record <- res 
            <div>
                <a identcreneaux=#{toPathPiece recordId} type=button .creneauxactiveeleve>
                    <span>#{dayFormat2 $ Just $ creneauxDateCreneaux record} (#{maybeText $ creneauxHeureCreneauxDebut record} - #{maybeText $ creneauxHeureCreneauxFin record})
    |]


showSpecifiqueCreneaux :: CreneauxId -> Widget
showSpecifiqueCreneaux creneauxid = do 
    res <- handlerToWidget $ runDB $ get404 creneauxid
    [whamlet|
        <span id=#{toPathPiece creneauxid}>#{dayFormat2 $ Just $ creneauxDateCreneaux res} (#{maybeText $ creneauxHeureCreneauxDebut res} - #{maybeText $ creneauxHeureCreneauxFin res})
    |]

showSpecifiqueCours :: CoursId -> Widget
showSpecifiqueCours coursid = do 
    res <- handlerToWidget $ runDB $ get404 coursid
    [whamlet|
        <span id=#{toPathPiece coursid}>#{coursEffectue res} 
    |]

showSpecifiqueEleve :: EleveId -> Widget
showSpecifiqueEleve eleveid = do 
    res <- handlerToWidget $ runDB $ get404 eleveid
    [whamlet|
        <div>
            <span>#{eleveNom res}
    |]

showSpecifiqueEnseignant :: EnseignantId -> Widget
showSpecifiqueEnseignant enseignantid = do 
    res <- handlerToWidget $ runDB $ get404 enseignantid
    [whamlet|
        <div>
            <span>#{enseignantNom res}
    |]
