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
getGraph   :: ClientM ResponseData


(getREADME :<|> initialize :<|> getGraph) = client restAPI 


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
	following <- liftIO $ followers' (En.decodeUtf8 (fromJust uname)) auth
	let firstFollowing = Data.List.head $ Data.List.tail $ Data.List.map follower_Rep_Text following 
        liftIO $ makeApiCall (DBC.unpack (fromJust access_token)) firstFollowing                        --(DBC.unpack (fromJust access_token)) (En.decodeUtf8 (fromJust uname))
        crawls <- liftIO $ getNode
        --let crawls = firstFollowing
        setTitle . toHtml $ En.decodeUtf8 (fromJust uname) <> "'s User page"
        $(widgetFile "profile")
		

makeApiCall ::  String -> Text -> IO ()
makeApiCall auth uname = liftIO $ do
  manager <- Network.HTTP.Client.newManager Network.HTTP.Client.defaultManagerSettings
  res <- runClientM (initialize (StartCrawl uname auth)) (ClientEnv manager (BaseUrl Http crawlerhost (read(crawlerport) :: Int) ""))
  case res of
    Left err -> Import.putStrLn $ "Error: "
    Right response -> return ()
	


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


-------------------------------------------------------------
-- Format user info into Rep data type [Followering]
------------------------------------------------------------
formatUsers ::  Maybe GHD.Auth -> GithubUsers.SimpleUser ->IO(Reps)
formatUsers auth repo = do
             let any = GithubUsers.untagName $ GithubUsers.simpleUserLogin repo
             return (Reps any)



 
-------------------------------------------------------------
-- Grab data from the database
------------------------------------------------------------      

getNode :: IO SocialGraph
getNode = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "09/12/1992" }
   result <- run pipe $ Database.Bolt.query (Data.Text.pack cypher) 
   result2 <- run pipe $ Database.Bolt.query (Data.Text.pack cypher2) 
   close pipe
   cruise1 <- mapM extractLink result
   cruise2 <- mapM extractNode result2
   return $ SocialGraph cruise2 cruise1
  where cypher = "MATCH (n) OPTIONAL MATCH path=(n)-[*1..2]-(c) WITH rels(path) AS rels UNWIND rels AS rel WITH DISTINCT rel RETURN startnode(rel).name as source, endnode(rel).name as target, type(rel) as type"
        cypher2 = "MATCH (n) OPTIONAL MATCH path=(n)-[r*1..2]-(c) where NONE( rel in r WHERE type(rel)='KNOWS') RETURN DISTINCT c.name as name, HEAD(LABELS(c)) as group"



extractNode :: Record -> IO UseHaskellAPI.Node        
extractNode input = do 
   cruise1 <- input `at` "name" >>= exact :: IO Text
   cruise2 <- input `at` "group" >>= exact :: IO Text
   return $ UseHaskellAPI.Node cruise1 cruise2



extractLink :: Record -> IO Links        
extractLink input = do 
   cruise1 <- input `at` "source" >>= exact :: IO Text
   cruise2 <- input `at` "target" >>= exact :: IO Text   
   cruise3 <- input `at` "type" >>= exact :: IO Text
   return $ Links cruise1 cruise2 cruise3

