module InitialPage where

import Lucid
import Pages

initialPage :: String -> Html ()
initialPage "" =
    base "Initial Playground" $ do
        form_ [class_ "ui form", method_ "post", action_ "/initial/submit"] $ do
            div_ [class_ "field"] $ do
                label_ [for_ "user-input"] "Initial Pet Maker"
                input_ [type_ "text", id_ "user-input", name_ "user-input"]
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        div_ [class_ "ui clearing divider"] ""
        nextButton "final"
initialPage ast =
    base "Playground" $ do
        form_ [class_ "ui form", method_ "post", action_ "/initial/submit"] $ do
            div_ [class_ "field"] $ do
                label_ [for_ "user-input"] "Initial Pet Maker"
                input_ [type_ "text", id_ "user-input", name_ "user-input"]
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        div_ [class_ "ui clearing divider"] ""
        h2_ [] "Output"
        toHtml ast
        div_ [class_ "ui clearing divider"] ""
        nextButton "final"
