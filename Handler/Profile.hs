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
import Data.List hiding(intercalate, lookup)
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
import qualified GitHub.Endpoints.Users as GithubUser
import GitHub as MainGitHub
import GitHub.Data as GHD
import GitHub.Data.Content as GHDC
import GitHub.Data.Repos as GHDR
import GitHub.Data.Name as GHDN
import Data.Bson.Generic
import qualified Data.Map as M hiding (split)
import Data.Text.IO as T (putStrLn)

data Rep = Rep{
        follower_name      :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

follower_Rep_Text :: Rep -> Text  
follower_Rep_Text (Rep follower) = follower


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

data UserInfo = UserInfo{
    user_name :: Text,
    user_url :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)


----------------------------------------------
--  Profile Handler
----------------------------------------------
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
        follow <- liftIO $ followers' (En.decodeUtf8 (fromJust uname)) auth
	let next_hop = Data.List.head $ Data.List.map follower_Rep_Text follow
	--deets <- liftIO $ repos (En.decodeUtf8 (fromJust uname))
	deets <- liftIO $ repos next_hop
	following <- liftIO $ followers' next_hop auth
        --content <- liftIO $ showUsers follow auth 
        --content <- liftIO $ readme (En.decodeUtf8 (fromJust uname))
	content <- liftIO $ readme next_hop

        setTitle . toHtml $ En.decodeUtf8 (fromJust uname) <> "'s User page"
        $(widgetFile "profile")



------------------------------------------------------------------------------
-- Get user's repo data taking in user's name
-----------------------------------------------------------------------------
repos :: Text -> IO[RepoData]
repos uname = do
  possibleRepos <- Github.userRepos (mkOwnerName uname) GHDR.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> return ([RepoData (Data.Text.Encoding.decodeUtf8 "Error") (Data.Text.Encoding.decodeUtf8 "Error")])
       (Right repos) -> do
         x <- mapM formatRepo repos
         return (V.toList x)

-------------------------------------------------------------------------
-- Format repo data - using repo data type
-------------------------------------------------------------------------
formatRepo :: Github.Repo -> IO(RepoData)
formatRepo repo = do
	let repoName = untagName (GHDR.repoName repo)
	let repoOwer = untagName $ simpleOwnerLogin (GHDR.repoOwner repo)
	return (RepoData repoName repoOwer)

	   
--------------------------------------------------------------------------
-- Get readme file data from username - TODO pass repo name as a variable
--------------------------------------------------------------------------
readme :: Text ->  IO[RepoContent]
readme owner = do
     let repo = Data.Text.Encoding.decodeUtf8 "CS7009"
     possiblereadmes <- Github.readmeFor (mkOwnerName owner) (mkRepoName repo)
     case possiblereadmes of
        (Left error)  -> return ([RepoContent (Data.Text.Encoding.decodeUtf8 "Error")])
	(Right (Github.ContentFile cd))  -> return ([RepoContent (Data.Text.pack (show cd))])


-------------------------------------------------------------------
-- Following function - returns data types rep for easy printing
--------------------------------------------------------------------
followers ::  Text -> Maybe GHD.Auth -> IO[Rep] 
followers uname auth  = do
    possibleUsers <- GitHub.executeRequestMaybe auth $ GitHub.usersFollowingR (mkUserName uname) GitHub.FetchAll 
    case possibleUsers of
        (Left error)  -> return ([Rep (Data.Text.Encoding.decodeUtf8 "Error")])
	(Right  repos) -> do
           x <- mapM formatUser repos
           return (V.toList x)

----------------------------------------------------------------
-- Followers function - returns data types rep for easy printing
----------------------------------------------------------------
followers' ::  Text -> Maybe GHD.Auth -> IO[Rep] 
followers' uname auth  = do
    possibleUsers <- GitHub.executeRequestMaybe auth $ GitHub.usersFollowedByR (mkUserName uname) GitHub.FetchAll 
    case possibleUsers of
        (Left error)  -> return ([Rep (Data.Text.Encoding.decodeUtf8 "Error")])
	(Right  repos) -> do
           x <- mapM formatUser repos
           return (V.toList x)
	   
----------------------------------------------
-- Format user info into Rep data type
---------------------------------------------
formatUser ::  GithubUsers.SimpleUser -> IO(Rep)
formatUser repo = do
             let any = GithubUsers.untagName $ GithubUsers.simpleUserLogin repo
	     return (Rep any)



-----------------------------------------------
--Show users details function 
-----------------------------------------------
showUsers ::  [Rep] -> Maybe GHD.Auth -> IO(UserInfo)
showUsers rep auth  = do
  let uname = Data.List.head $ Data.List.tail $ Data.List.map follower_Rep_Text rep
  possibleUser <- GithubUser.userInfoFor' auth (mkUserName uname)
  case possibleUser of
        (Left error)  -> return (UserInfo (Data.Text.Encoding.decodeUtf8 "Error")(Data.Text.Encoding.decodeUtf8 "Error"))
	(Right use)   -> do
           x <- formatUserInfo use
           return x



-------------------------------------------------
-- TODO - function to take [Rep] list and output
-- [userInfo] data
--
-- 1. apply follow_rep_txt function using map get
-- a [] of Text data from [Rep].
-- 2. apply map to showUsers
-- 3. ouput [UserInfo] data and display it
-------------------------------------------------

  



-----------------------------------------------
--format data into data type UserInfo 
-----------------------------------------------
formatUserInfo ::  GithubUser.User -> IO(UserInfo)
formatUserInfo user = do
         let userName =  GithubUser.userName user
         let logins =  GithubUser.userLogin user
	 let htmlUrl = GithubUser.userHtmlUrl user
	 let htmlUser = GithubUser.getUrl htmlUrl
	 let login =  GithubUser.untagName logins
         return (UserInfo login htmlUser)
  



