
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Translate
    ( module Translate
    ) where
import Text.Read
import Text.Show

import qualified Data.Text as T
{-
import Network.HTTP.Types 
    ( status200
    , status201
    , status400
    , status403
    , status404
    )
    -}
--import Data.Maybe 
   -- (fromJust)


-- LANGUES
-- Next we define the custom messages present on the site and their
-- rendering functions for different languages.
data Translation = TitleSource
   | TitleProprietaire
   | TitleEmplacement
   | TitleExhibition
   | TitleOeuvre
   | TitleAdd
   | TitleFournisseur
   | FieldName
   | FieldReliability
   | FieldKind
   | FieldCity
   | FieldCountry
   | FieldStartDate
   | FieldEndDate
   | FieldDate
   | FieldExhibitionName
   | FieldTitle
   | FieldCatalogue
   | FieldUnknow
   | NavbarHome
   | NavbarCreate
   | NavbarAddNewInformation
   | NavbarLogin
   | NavbarLogout
   | NavbarProfile
   | NavbarExhibition
   | NavbarLangue
   | NavbarHelp
   | NavbarUsers
   | NavbarSearch
   | SitePrivate
   | SiteMuseum
   | SiteWorkShop
   | SiteStore
   | SiteDomestic
   | Description
   | Link
   | Author
   | Format
   | Width
   | Height
   | TitleCreate
   | Painting
   | Owner
   | Exhibition
   | Location
   | Source
   | Supplier
   | TitleHOwner
   | TitleHLocation
   | FieldConservationCond
   | FieldNameExhibition
   | FieldAccurateStartDate
   | FieldChronologyStartDate
   | FieldAccurateEndDate
   | FieldChronologyEndDate
        deriving (Show, Read)
-- instance Show Translation where
--     show NavbarHelp = "Admin"
--     show NavbarUsers = "Responsable de catalogue"
--     show NavbarSearch = "Expert"    
--     show _ = "autre"    



-- Rendering function for English.
renderEnglish :: Translation -> T.Text
renderEnglish TitleSource = "Create a source"
renderEnglish TitleProprietaire = "Create an owner"
renderEnglish TitleEmplacement = "Create a location"
renderEnglish TitleExhibition = "Create an exhibition"
renderEnglish TitleOeuvre = "Create a work"
renderEnglish TitleAdd = "Add"
renderEnglish TitleFournisseur = "Create a supplier"
renderEnglish FieldName = "Name"
renderEnglish FieldReliability = "Reliability"
renderEnglish FieldKind = "Type of collection"
renderEnglish FieldCity = "City"
renderEnglish FieldCountry = "Country"
renderEnglish FieldStartDate = "From"
renderEnglish FieldEndDate = "To"
renderEnglish FieldDate = "Date of making "
renderEnglish FieldExhibitionName = "Exhibition name"
renderEnglish FieldTitle = "Title"
renderEnglish FieldCatalogue = "N° of catalogue"
renderEnglish FieldUnknow = "Unknow"
renderEnglish NavbarHome = "Home"
renderEnglish NavbarCreate = "Create"
renderEnglish NavbarAddNewInformation = "Add new information"
renderEnglish NavbarLogin = "Login"
renderEnglish NavbarLogout = "Logout"
renderEnglish NavbarProfile = "Profile"
renderEnglish NavbarExhibition = "Exhibition"
renderEnglish NavbarLangue = "Language"
renderEnglish NavbarHelp = "Help"
renderEnglish NavbarUsers = "Users"
renderEnglish NavbarSearch = "Search a painting"
renderEnglish SitePrivate = "Private"
renderEnglish SiteMuseum = "Museum"
renderEnglish SiteWorkShop = "Studio"
renderEnglish SiteStore = "Storage"
renderEnglish SiteDomestic = "Home"
renderEnglish Description = "Description"
renderEnglish Link = "Link"
renderEnglish Author = "Author"
renderEnglish Format = "Format"
renderEnglish Width = "Width"
renderEnglish Height = "Length"
renderEnglish TitleCreate = "Add a data about : "
renderEnglish Painting = "Painting"
renderEnglish Owner = "Owner"
renderEnglish Exhibition = "Exhibition"
renderEnglish Location = "Location"
renderEnglish Source = "Source"
renderEnglish Supplier = "Supplier"
renderEnglish TitleHOwner = "Add a new owner for this painting"
renderEnglish TitleHLocation = "Add a new location for this painting"
renderEnglish FieldConservationCond = "Conservation condition"
renderEnglish FieldNameExhibition = "Name of exhibition"
renderEnglish FieldAccurateStartDate = "Accurate of begin date"
renderEnglish FieldChronologyStartDate = "Chronology of begin date"
renderEnglish FieldAccurateEndDate = "Accurate of end date"
renderEnglish FieldChronologyEndDate = "Chronology end date"

-- Rendering function for french.
renderfrench :: Translation -> T.Text
renderfrench TitleSource = "Créer une source"
renderfrench TitleProprietaire = "Créer un propriétaire"
renderfrench TitleEmplacement = "Créer un emplacement"
renderfrench TitleExhibition = "Créer une exposition"
renderfrench TitleOeuvre = "Créer une fiche / œuvre"
renderfrench TitleAdd = "Ajouter"
renderfrench TitleFournisseur = "Créer un fournisseur"
renderfrench FieldName = "Nom"
renderfrench FieldReliability = "Fiabilité"
renderfrench FieldKind = "Type de collection"
renderfrench FieldCity = "Ville"
renderfrench FieldCountry = "Pays"
renderfrench FieldStartDate = "Date de début"
renderfrench FieldEndDate = "Date de fin"
renderfrench FieldDate = "Date de création"
renderfrench FieldExhibitionName = "Nom de l'exposition"
renderfrench FieldTitle = "Titre"
renderfrench FieldCatalogue = "N° du catalogue raisonné"
renderfrench FieldUnknow = "Inconnu"
renderfrench NavbarHome = "Page principale"
renderfrench NavbarCreate = "Créer"
renderfrench NavbarAddNewInformation = "Ajouter une nouvelle information"
renderfrench NavbarLogin = "Se connecter"
renderfrench NavbarLogout = "Se déconnecter"
renderfrench NavbarProfile = "Profil"
renderfrench NavbarExhibition = "Exposition"
renderfrench NavbarLangue = "Langue"
renderfrench NavbarHelp = "Aide"
renderfrench NavbarUsers = "Gestion utilisateurs"
renderfrench NavbarSearch = "Rechercher une œuvre"
renderfrench SitePrivate = "Prive(e)"
renderfrench SiteMuseum = "Musée"
renderfrench SiteWorkShop = "Atelier"
renderfrench SiteStore = "Réserve"
renderfrench SiteDomestic = "Usage privé"
renderfrench Description = "Description"
renderfrench Link = "Lien"
renderfrench Author = "Auteur"
renderfrench Format = "Format"
renderfrench Width = "Hauteur"
renderfrench Height = "Largeur"
renderfrench TitleCreate = "Vous souhaitez ajouter une information concernant : "
renderfrench Painting = "Œuvre"
renderfrench Owner = "Propriétaire"
renderfrench Exhibition = "Exposition"
renderfrench Location = "Emplacement (lieu de stockage et exposition)"
renderfrench Source = "Source"
renderfrench Supplier = "Fournisseur"
renderfrench TitleHOwner = "Ajouter un nouveau propriétaire à cette œuvre"
renderfrench TitleHLocation = "Ajouter un nouvel emplacement à cette œuvre"
renderfrench FieldConservationCond = "Etat de conservation"
renderfrench FieldNameExhibition = "Nom de l'exposition"
renderfrench FieldAccurateStartDate = "Précision de la date de début"
renderfrench FieldChronologyStartDate = "Chronologie de la date de début"
renderfrench FieldAccurateEndDate = "Précision de la date de fin"
renderfrench FieldChronologyEndDate = "Chronologie de la date de fin"



{-instance RenderMessage App Translation where
    --renderMessage :: App -> [Lang] -> FormMessage -> Text
    --renderMessage _ _ = defaultFormMessage
    renderMessage _ []        = renderEnglish-- Default to English
    renderMessage _ ("fr":_) = renderfrench -- French
    renderMessage _ ("en":_) = renderEnglish -- English
    renderMessage m (_   :ls) = renderMessage m ls

-}