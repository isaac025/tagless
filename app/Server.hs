{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Initial (parser)
import Lucid
import Network.Wai.Handler.Warp (run)
import Pages
import Servant
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm (..), parseUnique)

type Intro = "intro" :> Get '[HTML] (Html ())
type Lesson = "lesson" :> Get '[HTML] (Html ())
type BackgroundAPI = "what-is-it-about" :> Get '[HTML] (Html ())
type Playground =
    "playground"
        :> ( Get '[HTML] (Html ())
                :<|> "initial" :> ReqBody '[FormUrlEncoded] InitialForm :> Post '[HTML] (Html ())
           )

newtype InitialForm = InitialForm {initial :: Text}
    deriving (Generic)

instance FromForm InitialForm where
    fromForm f = InitialForm <$> parseUnique "initial" f

type FileAPI = "static" :> Raw

type API = Intro :<|> Lesson :<|> BackgroundAPI :<|> Playground :<|> FileAPI

server :: Server API
server = intro :<|> lesson :<|> background :<|> (playground :<|> playgroundPost) :<|> serveDirectoryFileServer "static"
  where
    intro :: Handler (Html ())
    intro = pure introPage

    lesson :: Handler (Html ())
    lesson = pure lessonPage

    background :: Handler (Html ())
    background = pure backgrounPage

    playground :: Handler (Html ())
    playground = pure $ playgroundPage ""

    playgroundPost :: InitialForm -> Handler (Html ())
    playgroundPost initialForm = do
        let parsed = parser (initial initialForm)
        case parsed of
            Left err -> pure $ playgroundPage (show err)
            Right ast -> pure $ playgroundPage (show ast)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runApp :: IO ()
runApp = run 8080 app
