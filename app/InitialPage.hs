module InitialPage where

import Lucid
import Pages

initialPage :: String -> Html ()
initialPage "" =
    base "Initial Playground" $ do
        form_ [method_ "post", action_ "/initial/submit"] $ do
            label_ [for_ "user-input"] "Initial"
            textarea_ [id_ "user-input", name_ "user-input", style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        div_ [class_ "ui clearing divider"] ""
        nextButton "final"
initialPage ast =
    base "Playground" $ do
        form_ [method_ "post", action_ "/initial/submit"] $ do
            label_ [for_ "user-input"] "Initial"
            textarea_ [id_ "user-input", name_ "user-input", style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        div_ [class_ "ui clearing divider"] ""
        h2_ [] "Output"
        toHtml ast
        div_ [class_ "ui clearing divider"] ""
        nextButton "final"
