{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Handler.Profile where

import Import hiding (unpack, pack)
import Data.List hiding(intercalate, map, lookup)
import qualified GitHub.Endpoints.Repos as Github
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import Data.Maybe
import Data.Aeson
import Data.Text.Encoding
import Data.Vector as V hiding (mapM)
import Data.Text hiding(intercalate, map, lookup)


data RepoData = RepoData{
	repo_name :: Text,
	repo_owner :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
    	let access_token = lookup "access_token" sess
    	let uname = lookup "login" sess
        deets <- liftIO $ repos (Data.Text.Encoding.decodeUtf8 (fromJust uname))
        setTitle . toHtml $ Data.Text.Encoding.decodeUtf8 (fromJust uname) <> "'s User page"
        $(widgetFile "profile")

repos :: Text -> IO[RepoData]
repos uname = do
  possibleRepos <- Github.userRepos (mkOwnerName uname) GHDR.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> return ([RepoData (Data.Text.Encoding.decodeUtf8 "Error") (Data.Text.Encoding.decodeUtf8 "Error")])
       (Right repos) -> do
         x <- mapM formatRepo repos
         return (V.toList x)

formatRepo :: Github.Repo -> IO(RepoData)
formatRepo repo = do
	let repoName = untagName (GHDR.repoName repo)
	let repoOwer = untagName $ simpleOwnerLogin (GHDR.repoOwner repo)
	return (RepoData repoName repoOwer)
        
  {-unpack (untagName (GHDR.repoName repo)) Data.List.++ "\t" Data.List.++
    unpack (fromMaybe "" $ (GHDR.repoDescription repo)) Data.List.++ "\n" Data.List.++
    unpack (GHDR.repoHtmlUrl repo) Data.List.++ "\n" Data.List.++
    unpack (fromMaybe "" $ (GHDR.repoCloneUrl repo)) Data.List.++ "\t" Data.List.++
    --(formatDate $ Github.repoUpdatedAt repo) Data.List.++ "\n" Data.List.++
    formatLanguage (GHDR.repoLanguage repo) Data.List.++
    "watchers: " Data.List.++ (show $ GHDR.repoWatchers repo) Data.List.++ "\t" Data.List.++
    "forks: " Data.List.++ (show $ GHDR.repoForks repo)
--formatDate (Just date) = show . Github.fromDate $ date
--formatDate Nothing = ""
formatLanguage (Just language) = "language: " Data.List.++ (unpack (getLanguage language)) Data.List.++ "\t"
formatLanguage Nothing = ""-}