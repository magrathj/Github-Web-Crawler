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
import Data.ByteString.Char8 as DBC hiding (putStrLn)
import qualified GitHub.Endpoints.Repos as Github
import qualified GitHub.Endpoints.Users.Followers as GithubUsers
import qualified GitHub.Endpoints.Users as GithubUser
import GitHub as MainGitHub
import GitHub.Data as GHD
import GitHub.Data.Content as GHDC
import GitHub.Data.Repos as GHDR
import GitHub.Data.Name as GHDN
import Data.Bson.Generic
import Data.Bits
import Data.Char
import qualified Data.Map as M hiding (split)
import Data.Text.IO as T (putStrLn)
import Control.Monad (when, liftM)
import Database.Bolt
import Data.Text
import qualified Data.Map as DM
import qualified Database.Bolt as Neo
import qualified Data.Text as DT
import GHC.Generics
import qualified Servant as S
import Servant.API
import Servant.Client
import qualified Servant.Server as SS
import UseHaskellAPI
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Prelude (read)

data Reps = Reps{
        follower_name      :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

follower_Rep_Text :: Reps -> Text  
follower_Rep_Text (Reps follower) = follower




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
    user_location ::Text,
	user_email :: Text,
	user_company :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)


getUserName :: UserInfo -> Text
getUserName (UserInfo name _ _ _ _) = name

getUserUrl :: UserInfo -> Text
getUserUrl (UserInfo _ url _ _ _) = url

getUserLocation :: UserInfo -> Text
getUserLocation (UserInfo _ _ loc _ _) = loc

getUserEmail :: UserInfo -> Text
getUserEmail (UserInfo _ _ _ em _) = em

getUserCompany :: UserInfo -> Text
getUserCompany (UserInfo _ _ _ _ com) = com

----------------------------------------------
--  Declare Crawler API
----------------------------------------------

crawlerport :: String
crawlerport = "8080"

crawlerhost :: String
crawlerhost = "localhost"

----------------------------------------------
--  Declare Crawler API
----------------------------------------------

restAPI :: S.Proxy API
restAPI = S.Proxy




getREADME   :: ClientM ResponseData
initialize  :: StartCrawl -> ClientM ResponseData



(getREADME :<|> initialize) = client restAPI 


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
	let auth = Just $ MainGitHub.OAuth $ fromJust access_token
	follow <- liftIO $ followers' (En.decodeUtf8 (fromJust uname)) auth
	--let crawls = Data.List.head $ Data.List.map follower_Rep_Text follow
	--crawls <- liftIO $ getNode 
	liftIO $ crawler auth (En.decodeUtf8 (fromJust uname))   --crawl 
        liftIO $ makeApiCall (DBC.unpack (fromJust access_token)) (En.decodeUtf8 (fromJust uname))
        crawls <- liftIO $ getNode
        setTitle . toHtml $ En.decodeUtf8 (fromJust uname) <> "'s User page"
        $(widgetFile "profile")
		

makeApiCall ::  String -> Text -> IO ()
makeApiCall auth uname = liftIO $ do
  manager <- Network.HTTP.Client.newManager Network.HTTP.Client.defaultManagerSettings
  res <- runClientM (initialize (StartCrawl uname auth)) (ClientEnv manager (BaseUrl Http crawlerhost (read(crawlerport) :: Int) ""))
  case res of
    Left err -> Import.putStrLn $ "Error: "
    Right response -> return ()
	

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
followers ::  Maybe GHD.Auth -> Text -> IO[Reps] 
followers auth uname = do
    possibleUsers <- GitHub.executeRequestMaybe auth $ GitHub.usersFollowingR (mkUserName uname) GitHub.FetchAll 
    case possibleUsers of
        (Left error)  -> return ([Reps (Data.Text.Encoding.decodeUtf8 "Error")])
	(Right  repos) -> do
           x <- mapM (formatUser auth) repos
           return (V.toList x)

----------------------------------------------------------------
-- Followers function - returns data types rep for easy printing
----------------------------------------------------------------
followers' ::  Text -> Maybe GHD.Auth -> IO[Reps] 
followers' uname auth  = do
    possibleUsers <- GitHub.executeRequestMaybe auth $ GitHub.usersFollowedByR (mkUserName uname) GitHub.FetchAll 
    case possibleUsers of
        (Left error)  -> return ([Reps (Data.Text.Encoding.decodeUtf8 "Error")])
	(Right  repos) -> do
           x <- mapM (formatUsers auth) repos
           return (V.toList x)



----------------------------------------------
-- Format user info into Rep data type [CRAWLER]
---------------------------------------------
formatUser ::  Maybe GHD.Auth -> GithubUsers.SimpleUser ->IO(Reps)
formatUser auth repo = do
             let any = GithubUsers.untagName $ GithubUsers.simpleUserLogin repo
	     crawler auth any 
             return (Reps any)

----------------------------------------------
-- Format user info into Rep data type [Followering]
---------------------------------------------
formatUsers ::  Maybe GHD.Auth -> GithubUsers.SimpleUser ->IO(Reps)
formatUsers auth repo = do
             let any = GithubUsers.untagName $ GithubUsers.simpleUserLogin repo
             return (Reps any)






-----------------------------------------------
--Show users details function 
-----------------------------------------------
showUsers ::  Text -> Maybe GHD.Auth -> IO(UserInfo)
showUsers uname auth  = do
  --let uname = Data.List.head $ Data.List.tail $ Data.List.map follower_Rep_Text rep
  possibleUser <- GithubUser.userInfoFor' auth (mkUserName uname)
  case possibleUser of
        (Left error)  -> return (UserInfo (Data.Text.Encoding.decodeUtf8 "Error")(Data.Text.Encoding.decodeUtf8 "Error")( Data.Text.Encoding.decodeUtf8 "Error")( Data.Text.Encoding.decodeUtf8 "Error")( Data.Text.Encoding.decodeUtf8 "Error"))
	(Right use)   -> do
           x <- formatUserInfo use
           return x





-----------------------------------------------------------------
--  Crawler function 
----------------------------------------------------------------
crawler ::  Maybe GHD.Auth -> Text -> IO()
crawler auth unamez = do
  if (Data.Text.null unamez) == True then return ()
   else do
      checkDB <- lookupNodeNeo unamez
      case checkDB of
        False -> return()   -- Isnt empty, so already there
        True -> do
	       inputDB <-  liftIO $ testFunction unamez
               userDets <- liftIO $ showUsers unamez auth	
               let userLogin = getUserName userDets
               let userUrl = getUserUrl userDets
               let userLocation = getUserLocation userDets
               let userEmail = getUserEmail userDets
               let userCompany = getUserCompany userDets
               let followings = followers auth unamez
	       followings2 <- liftIO $ followings
               let follow_text = Data.List.map follower_Rep_Text followings2	
               liftIO $ insertUserDets 	unamez userLogin userUrl userLocation userEmail userCompany		    
               let checkList = Data.List.null followings2
               case checkList of 
		           True -> return ()
			   False -> do
			        input2DB <- liftIO $ mapM (insertFollowers unamez) follow_text
                                return ()
     
 





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
	 let emailwithMaybe = GitHub.userEmail user
	 let email = fromMaybe "" emailwithMaybe
	 let companywtihMaybe = GitHub.userCompany user
	 let company = fromMaybe "" companywtihMaybe
         return (UserInfo login htmlUser userlocation email company)
  



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
---  add attribute to the database
--------------------------------------------------------------
insertFollower :: Text -> Text -> IO [Record]
insertFollower userName userFollowers = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   close pipe
   return result
 where cypher = "MATCH (n { name: {userName} }) SET n += {followers: {userFollowers}} RETURN n"
       params = DM.fromList [("userName", Database.Bolt.T userName),("userFollowers", Database.Bolt.T userFollowers)]
--------------------------------------------------------------
---  add attribute to the database
--------------------------------------------------------------
insertUserDets :: Text -> Text -> Text -> Text -> Text -> Text -> IO ()
insertUserDets userName userLogin userUrl userLocation userEmail userCompany = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   close pipe
   return ()
 where cypher = "MATCH (n { name: {userName} }) SET n += {location: {userLocation}} SET n += {url: {userUrl}} SET n += {Login: {userLogin}} SET n += {email: {userEmail}} SET n += {company: {userCompany}} RETURN n"
       params = DM.fromList [("userName", Database.Bolt.T userName),("userLocation", Database.Bolt.T userLocation),("userUrl", Database.Bolt.T userUrl),("userLogin", Database.Bolt.T userLogin),("userEmail", Database.Bolt.T userEmail),("userCompany", Database.Bolt.T userCompany)]



--------------------------------------------------------------
---  add attribute to the database
--------------------------------------------------------------
insertFollowers :: Text -> Text -> IO [Record]
insertFollowers userName userFollowers = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   close pipe
   return result
 where cypher = "MATCH (n:User { name: {userName} }) CREATE (w:Fol {name: {userFollowers}}) MERGE (n)<-[r:FOLLOWS]-(w) RETURN n"
       params = DM.fromList [("userName", Database.Bolt.T userName),("userFollowers", Database.Bolt.T userFollowers)]





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


getNodesWithLinks :: IO [Record]
getNodesWithLinks = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.query (Data.Text.pack cypher) 
   close pipe
   --nodeData <- mapM toNode result
   --linkData <- mapM (recordat (Data.Text.Encoding.decodeUtf8 "source")) result
   return result
  where cypher = "MATCH (n) OPTIONAL MATCH path=(n)-[*1..2]-(c) WITH rels(path) AS rels UNWIND rels AS rel WITH DISTINCT rel RETURN startnode(rel).name as source, endnode(rel).name as target, type(rel) as type"
        
data Node = Node{
  name :: Text
} deriving(ToJSON, FromJSON, Generic, Eq, Show)
--instance FromBSON String
--instance ToBSON String


getNode :: IO [Handler.Profile.Node]
getNode = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.query (Data.Text.pack cypher) 
   close pipe
   cruise <- mapM extractNode result
   return cruise
  where cypher = "MATCH (n) OPTIONAL MATCH path=(n)-[*1..2]-(c) WITH rels(path) AS rels UNWIND rels AS rel WITH DISTINCT rel RETURN startnode(rel).name as source, endnode(rel).name as target, type(rel) as type"

extractNode :: Record -> IO Handler.Profile.Node        
extractNode input = do 
   cruise <- input `at` "source" >>= exact :: IO Text
   return $ Handler.Profile.Node cruise

setRelationships :: IO ()
setRelationships = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.query (Data.Text.pack cypher) 
   close pipe
   return ()
  where cypher = "MATCH (n:User) MATCH (m:Fol ) WHERE n.name = m.name MERGE (n)-[:FOLLOWS]->(m)"

setFriendshipRelationships :: IO ()
setFriendshipRelationships = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.query (Data.Text.pack cypher) 
   close pipe
   return ()
  where cypher = "MATCH p = (a:User) --> (b) --> (c:User) MATCH z = (d:User) --> (e) --> (f:User) WHERE a.name = f.name AND d.name = c.name CREATE UNIQUE (a)-[r:FRIENDS]->(c)"

getFriends :: IO()
getFriends = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.query (Data.Text.pack cypher) 
   close pipe
   return ()
 where cypher = "MATCH (n:User)<-[r:FRIENDS]-(p:User) RETURN n,r,p"


