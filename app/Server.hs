{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Text.Lazy (Text)
import Final (arithParser, logicParser)
import FinalPage
import GHC.Generics (Generic)
import Initial (parser)
import InitialPage
import Lucid
import Network.Wai.Handler.Warp (run)
import Pages
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
                :<|> "submit" :> "arith" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] (Html ())
                :<|> "submit" :> "logic" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] (Html ())
           )

newtype Form = Form {userInput :: Text}
    deriving (Generic)

instance FromForm Form where
    fromForm f = Form <$> parseUnique "user-input" f

type FileAPI = "static" :> Raw

type API = Intro :<|> Lesson :<|> BackgroundAPI :<|> Initial :<|> Final :<|> FileAPI

server :: Server API
server = intro :<|> lesson :<|> background :<|> (initial :<|> initialPost) :<|> (final :<|> finalArithPost :<|> finalLogicPost) :<|> serveDirectoryFileServer "static"
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
        let parsed = parser (userInput form)
        case parsed of
            Left err -> pure $ initialPage (show err)
            Right ast -> pure $ initialPage (show ast)

    final :: Handler (Html ())
    final = pure $ finalPage ""

    finalArithPost :: Form -> Handler (Html ())
    finalArithPost form = do
        let parsed = arithParser (userInput form) :: Either String Int
        case parsed of
            Left err -> pure $ finalPage (show err)
            Right ast -> pure $ finalPage (show ast)

    finalLogicPost :: Form -> Handler (Html ())
    finalLogicPost form = do
        let parsed = logicParser (userInput form) :: Either String Bool
        case parsed of
            Left err -> pure $ finalPage (show err)
            Right ast -> pure $ finalPage (show ast)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runApp :: IO ()
runApp = run 8080 app
