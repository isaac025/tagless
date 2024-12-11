{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Text.Lazy (Text)
import FinalPage
import GHC.Generics (Generic)
import InitialPage
import Lucid
import Network.Wai.Handler.Warp (run)
import Pages
import Pet
import Servant
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm (..), parseUnique)

type Intro = "intro" :> Get '[HTML] (Html ())
type Lesson = "lesson" :> Get '[HTML] (Html ())
type BackgroundAPI = "what-is-it-about" :> Get '[HTML] (Html ())
type Initial =
    "initial"
        :> ( Get '[HTML] (Html ())
                :<|> "submit" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] (Html ())
           )
type Final =
    "final"
        :> ( Get '[HTML] (Html ())
                :<|> "submit" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] (Html ())
           )

newtype Form = Form {userInput :: Text}
    deriving (Generic)

instance FromForm Form where
    fromForm f = Form <$> parseUnique "user-input" f

type FileAPI = "static" :> Raw

type API = Intro :<|> Lesson :<|> BackgroundAPI :<|> Initial :<|> Final :<|> FileAPI

server :: Server API
server = intro :<|> lesson :<|> background :<|> (initial :<|> initialPost) :<|> (final :<|> finalPost) :<|> serveDirectoryFileServer "static"
  where
    intro :: Handler (Html ())
    intro = pure introPage

    lesson :: Handler (Html ())
    lesson = pure lessonPage

    background :: Handler (Html ())
    background = pure backgroundPage

    initial :: Handler (Html ())
    initial = pure $ initialPage ""

    initialPost :: Form -> Handler (Html ())
    initialPost form = do
        let parsed = initialParser (userInput form)
        case parsed of
            Left err -> pure $ initialPage (show err)
            Right ast -> pure $ initialPage (show ast)

    final :: Handler (Html ())
    final = pure $ finalPage (Pet "default-name" "idle")

    finalPost :: Form -> Handler (Html ())
    finalPost form = do
        let parsed = finalParser (userInput form) :: Either String (PetM Text)
        case parsed of
            Left _ -> pure $ finalPage (Pet "default-name" "idle")
            Right ast -> do
                let a = runPet ast (Pet "default-name" "idle")
                pure $ finalPage a

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runApp :: IO ()
runApp = run 8080 app
