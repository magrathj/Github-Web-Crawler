{-# LANGUAGE OverloadedStrings #-}
module Handler.Profile where

import Import hiding (unpack, pack)
import Data.List hiding(intercalate, map, lookup)
import qualified GitHub.Endpoints.Repos as Github
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import Data.Maybe
import Data.Text.Encoding
import Data.Text hiding(intercalate, map, lookup)

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
        let access_token = lookup "access_token" sess
    	let uname = lookup "login" sess
    --    deets <- repoDetails uname
        setTitle . toHtml $ (show access_token)  <> "'s User page"
        $(widgetFile "profile")