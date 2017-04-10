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

data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)



-- | We will also define a simple data type for returning data from a REST call, again with nothing special or
-- particular in the response, but instead merely as a demonstration.

data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

data StartCrawl = StartCrawl     { start :: Text
                                  ,authentication :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)


type API = "getREADME"                  :> Get '[JSON] ResponseData
      :<|> "initialize"                 :> ReqBody '[JSON] StartCrawl  :> Post '[JSON] ResponseData
     
