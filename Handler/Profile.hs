module Handler.Profile where

import Import
import Yesod.Auth

getProfileR :: Handler Html
getProfileR = do
	maid <- maybeAuthId
	defaultLayout $ do
		setTitle "Welcome!"
		$(widgetFile "profile")
