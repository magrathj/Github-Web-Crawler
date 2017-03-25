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
import qualified GitHub.Endpoints.Users as GithubUsers
import GitHub as MainGitHub
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import GitHub.Data.Name as GHDN

data RepoData = RepoData{
	repo_name      :: Text,
	repo_owner     :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

data GithubOwner = GithubOwner{
        follower_name      :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)


getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
    	let access_token = lookup "access_token" sess
    	let uname = lookup "login" sess
        let deets = lookup "login" sess
	let auth = Just $ MainGitHub.OAuth $ fromJust access_token
        --deets <- liftIO $ repos (En.decodeUtf8 (fromJust uname))
        -- deets <- liftIO $ followers
        setTitle . toHtml $ En.decodeUtf8 (fromJust uname) <> "'s User page"
        $(widgetFile "profile")

