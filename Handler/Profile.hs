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
--import qualified GitHub.Endpoints.Repos as Github
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import qualified GitHub
import Data.Maybe
import Data.Aeson
import Data.Text.Encoding
import Data.Vector as V hiding (mapM)
import Data.Text hiding(intercalate, map, lookup)
import Data.Text.Encoding as En
import Prelude ()
import Data.ByteString.Char8 as DBC hiding (unpack, putStrLn, find)
import qualified GitHub.Endpoints.Repos as Github
import qualified GitHub.Endpoints.Users.Followers as GithubUsers
import GitHub as MainGitHub
import GitHub.Data as GHD
import GitHub.Data.Content as GHDC
import GitHub.Data.Repos as GHDR
import GitHub.Data.Name as GHDN
import Data.Bson.Generic
import qualified Data.Map as M hiding (split)


data Rep = Rep{
        follower_name      :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)


data GithubOwner' = GithubOwner'{
        follower_name'      :: String
}deriving(ToJSON, FromJSON, Generic, Eq, Show)



data RepoData = RepoData{
	repo_name :: Text,
	repo_owner :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)



data RepoContent = RepoContent{
	repo_content :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)




getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
    	let access_token = lookup "access_token" sess
    	let uname = lookup "login" sess
       -- let deets = lookup "login" sess
	let auth = Just $ MainGitHub.OAuth $ fromJust access_token
	let userData = GithubOwner' (unpack $ Data.Text.Encoding.decodeUtf8 (fromJust uname))
        deets <- liftIO $ repos (En.decodeUtf8 (fromJust uname))
	content <- liftIO $ readme (En.decodeUtf8 (fromJust uname))
        setTitle . toHtml $ En.decodeUtf8 (fromJust uname) <> "'s User page"
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


readme :: Text ->  IO[RepoContent]
readme owner = do
     let repo = Data.Text.Encoding.decodeUtf8 "CS7009"
     possiblereadmes <- Github.readmeFor (mkOwnerName owner) (mkRepoName repo)
     case possiblereadmes of
        (Left error)  -> return ([RepoContent (Data.Text.Encoding.decodeUtf8 "Error")])
	(Right (Github.ContentFile cd))  -> return ([RepoContent (Data.Text.pack (show cd))])