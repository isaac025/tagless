module Pages where

import Lucid

base :: Html () -> Html ()
base h =
    doctypehtml_ $ do
        html_ $ do
            body_ $ do
                h

introPage :: Html ()
introPage =
    base $ do
        h1_ [] "Typed Tagless Final Interpreters"
        a_ [href_ "http://localhost:8080/initial"] "next"

initialPage :: Html ()
initialPage =
    base $ do
        h1_ [] "Initial Embedding"
        a_ [href_ "http://localhost:8080/final"] "next"

finalPage :: Html ()
finalPage =
    base $ do
        h1_ [] "Final Embedding"
        a_ [href_ "http://localhost:8080/playground"] "next"

playgroundPage :: Html ()
playgroundPage =
    base $ do
        h1_ [] "Playground"
        form_ [method_ "post", action_ ""] $ do
            label_ [] "Initial"
            textarea_ [] ""
            label_ [] "Final"
            textarea_ [] ""
            button_ [type_ "submit"] "Interpret"
