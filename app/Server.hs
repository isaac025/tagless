{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lucid
import Network.Wai.Handler.Warp (run)
import Pages
import Servant
import Servant.HTML.Lucid

type Intro = "intro" :> Get '[HTML] (Html ())
type Lesson = "lesson" :> Get '[HTML] (Html ())
type BackgroundAPI = "what-is-it-about" :> Get '[HTML] (Html ())
type Playground = "playground" :> Get '[HTML] (Html ())

type FileAPI = "static" :> Raw

type API = Intro :<|> Lesson :<|> BackgroundAPI :<|> Playground :<|> FileAPI

server :: Server API
server = intro :<|> lesson :<|> background :<|> playground :<|> serveDirectoryFileServer "static"
  where
    intro :: Handler (Html ())
    intro = pure introPage

    lesson :: Handler (Html ())
    lesson = pure lessonPage

    background :: Handler (Html ())
    background = pure backgrounPage

    playground :: Handler (Html ())
    playground = pure playgroundPage

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runApp :: IO ()
runApp = run 8080 app
