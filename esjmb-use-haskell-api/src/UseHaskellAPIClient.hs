{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}



module UseHaskellAPIClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           UseHaskellAPI
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

restAPI :: Proxy API
restAPI = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs


getGraphFollowers         :: ClientM SocialGraph
initialize                :: StartCrawl -> ClientM ResponseData
getGraph                  :: ClientM SocialGraph
getGraphFriends           :: ClientM SocialGraph
getDegreeDistribution     :: ClientM Degree
getClusterOfFriends       :: ClientM SocialGraph
getHighestDegreeNodes     :: ClientM SocialGraph
getFriendsByCompany       :: ClientM SocialGraph
getFriendsByLocation      :: ClientM SocialGraph

-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

(getGraphFollowers :<|> initialize :<|> getGraph :<|> getGraphFriends :<|> getDegreeDistribution :<|> getClusterOfFriends :<|> getHighestDegreeNodes :<|> getFriendsByCompany :<|> getFriendsByLocation) = client restAPI 
