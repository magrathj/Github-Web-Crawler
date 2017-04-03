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
import Control.Monad (when, liftM)
import Database.Bolt
import Data.Text
import qualified Data.Map as DM


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
    user_url :: Text,
    user_location ::Text
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
	let userData = GithubOwner' (Data.Text.unpack $ Data.Text.Encoding.decodeUtf8 (fromJust uname))
        follow <- liftIO $ followers' (En.decodeUtf8 (fromJust uname)) auth
	let next_hop = Data.List.head $ Data.List.tail $ Data.List.map follower_Rep_Text follow
	let next =  Data.List.map follower_Rep_Text follow
	deets <- liftIO $ repos (En.decodeUtf8 (fromJust uname))
	--deets <- liftIO $ repos next_hop
	--following <- liftIO $ followers' next_hop auth
	let uname_crawl = (En.decodeUtf8 (fromJust uname))
	let applyfollow = followers auth
        --following = Data.List.map applyfollow next
	--content <-  crawler [(En.decodeUtf8 (fromJust uname))] auth
        --content <- liftIO $ showUsers (En.decodeUtf8 (fromJust uname)) auth 
        --content <- liftIO $ readme (En.decodeUtf8 (fromJust uname))
        content <- liftIO $ lookupNodeNeo (En.decodeUtf8 (fromJust uname))
        --content <- liftIO $ testFunction next_hop
	following <- liftIO $ crawler auth next_hop
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
followers ::  Maybe GHD.Auth -> Text -> IO[Rep] 
followers auth uname = do
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
formatUser ::  GithubUsers.SimpleUser ->IO(Rep)
formatUser repo = do
             let any = GithubUsers.untagName $ GithubUsers.simpleUserLogin repo
             return (Rep any)



-----------------------------------------------
--Show users details function 
-----------------------------------------------
showUsers ::  Text -> Maybe GHD.Auth -> IO(UserInfo)
showUsers uname auth  = do
  --let uname = Data.List.head $ Data.List.tail $ Data.List.map follower_Rep_Text rep
  possibleUser <- GithubUser.userInfoFor' auth (mkUserName uname)
  case possibleUser of
        (Left error)  -> return (UserInfo (Data.Text.Encoding.decodeUtf8 "Error")(Data.Text.Encoding.decodeUtf8 "Error")( Data.Text.Encoding.decodeUtf8 "Error"))
	(Right use)   -> do
           x <- formatUserInfo use
           return x





-----------------------------------------------------------------
--  Crawler function 
----------------------------------------------------------------
crawler ::  Maybe GHD.Auth -> Text -> IO[Rep]
crawler auth unamez = do
  if (Data.Text.null unamez) == True then return $ [Rep (Data.Text.Encoding.decodeUtf8 "Error")]
   else do
      checkDB <- lookupNodeNeo unamez
      case checkDB of
        False -> return $ [Rep(Data.Text.Encoding.decodeUtf8 "already there")]    -- Is empty 
        True -> do
              let followings = followers auth unamez
	      followings2 <- liftIO $ followings
	   -- let text_following = Data.List.map follower_Rep_Text followings2
	   -- inputDB <- liftIO $ testFunction unamez
	   -- let mapCrawler = crawler auth
	    --mapCrawlerIO <- liftIO mapCrawler
	   -- mapM mapCrawler text_following
              return followings2 
     















checkList :: [Text] -> Maybe GHD.Auth -> IO[Text]
checkList list auth =
   case Data.List.null list of
      True -> return [] --[(Data.Text.Encoding.decodeUtf8 "True")]
      False -> return [] --[(Data.Text.Encoding.decodeUtf8 "False")]
      --( liftIO $ crawler (Data.List.head list) auth (Data.List.tail list))
  
		              -- Data.List.head $  
                              --let next_hop = Data.List.head $ Data.List.map follower_Rep_Text x
                              -- y <- liftIO $ showUsers next_hop auth
                              -- liftIO $ crawler next_hop auth uname
                      



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
	 let location = GithubUser.userLocation user
	 let userlocation = fromMaybe "" location
         return (UserInfo login htmlUser userlocation)
  



--------------------------------------------------------------
---  add attribute to the database
--------------------------------------------------------------
testFunction :: Text ->  IO [Record]
testFunction userName = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   close pipe
   return result
 where cypher = "CREATE (n:User {name: {userName}}) RETURN n"
       params = DM.fromList [("userName", Database.Bolt.T userName)]



--------------------------------------------------------------
---  Return boolean for match of data
--------------------------------------------------------------
lookupNodeNeo :: Text -> IO Bool
lookupNodeNeo userName = do
  let neo_conf = Database.Bolt.def { Database.Bolt.user = "neo4j", Database.Bolt.password = "09/12/1992" }
  neo_pipe <- Database.Bolt.connect $ neo_conf 

  -- -- Check node
  records <- Database.Bolt.run neo_pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params

  Database.Bolt.close neo_pipe

  let isEmpty = Data.List.null records
  return isEmpty

 where cypher = "MATCH (n { name: {userName} })RETURN n"
       params = DM.fromList [("userName", Database.Bolt.T userName)]




--------------------------------------------------------------
---  retrieve all data 
--------------------------------------------------------------

      




