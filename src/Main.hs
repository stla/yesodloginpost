{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main (
  main
) where

import Yesod
import Yesod.Static
import Data.Maybe (fromJust, isJust)
import Data.Text


users :: String -> Maybe String
users username = case username of
  "joe" -> Just "abc"
  "jack" -> Just "xyz"
  _ -> Nothing

staticFiles "static"

data App = App { getStatic :: Static }
instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
/app AppR POST
/static StaticR Static getStatic
|]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  mmsg <- getMessage
  addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
  addStylesheet $ StaticR login_css
  toWidget $(whamletFile "templates/login.hamlet")
  
postAppR :: Handler Html
postAppR = defaultLayout $ do
  _username <- lookupPostParam "username"
  _password <- lookupPostParam "password"
  let user = unpack (fromJust _username)
  let password = unpack (fromJust _password)
  let test = users user == Just password
  addStylesheet $ StaticR app_css
  case test of
    True -> toWidget $(whamletFile "templates/app.hamlet")
    False -> do 
      setMessage [shamlet|Wrong username or password|]
      redirect HomeR
  
main = do 
  static@(Static settings) <- static "static"
  warp 8000 $ App static


