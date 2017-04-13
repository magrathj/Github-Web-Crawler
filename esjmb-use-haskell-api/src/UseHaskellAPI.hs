{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module UseHaskellAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics 
import           Servant
import           Data.Text
import           Data.Text.Encoding
import           GitHub.Data as GHD
import           GitHub.Data.Repos as GHDR
import qualified GitHub
import qualified GitHub.Endpoints.Repos as Github
import qualified GitHub.Endpoints.Users.Followers as GithubUsers
import qualified GitHub.Endpoints.Users as GithubUser
import           GitHub as MainGitHub
import           GitHub.Data as GHD
import           GitHub.Data.Content as GHDC
import           GitHub.Data.Repos as GHDR
import           GitHub.Data.Name as GHDN


data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

data StartCrawl = StartCrawl     { start :: Text
                                  ,authentication :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

			 
data Node = Node{
  name :: String,
  group :: String
} deriving(ToJSON, FromJSON, Generic, Eq, Show)


data Links = Links{
                 source :: String,
                 target :: String,
                  value :: String
}deriving(ToJSON, FromJSON, Generic, Eq, Show)


data SocialGraph = SocialGraph{   nodes :: [Node],
	                              links :: [Links]
                               }deriving(ToJSON, FromJSON, Generic, Eq, Show)

data Degree = Degree{
                      degree       :: [String],
                      distribution :: [Int]
}deriving(ToJSON, FromJSON, Generic, Eq, Show)


type API = "getREADME"                  :> Get '[JSON] ResponseData
      :<|> "initialize"                 :> ReqBody '[JSON] StartCrawl  :> Post '[JSON] ResponseData
      :<|> "getGraph"                   :> Get '[JSON] SocialGraph 
	  :<|> "getGraphFriends"            :> Get '[JSON] SocialGraph 
      :<|> "getDegreeDistribution"      :> Get '[JSON] Degree 