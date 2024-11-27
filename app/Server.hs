{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lucid
import Network.Wai.Handler.Warp (run)
import Pages
import Servant
import Servant.HTML.Lucid

type Intro = "intro" :> Get '[HTML] (Html ())
type Initial = "initial" :> Get '[HTML] (Html ())
type Final = "final" :> Get '[HTML] (Html ())
type Playground = "playground" :> Get '[HTML] (Html ())

type API = Intro :<|> Initial :<|> Final :<|> Playground

server :: Server API
server = intro :<|> initial :<|> final :<|> playground
  where
    intro :: Handler (Html ())
    intro = pure introPage

    initial :: Handler (Html ())
    initial = pure initialPage

    final :: Handler (Html ())
    final = pure finalPage

    playground :: Handler (Html ())
    playground = pure playgroundPage

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runApp :: IO ()
runApp = run 8080 app
