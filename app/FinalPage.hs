module FinalPage where

import Lucid
import Pages

finalPage :: String -> Html ()
finalPage "" =
    base "Final Playground" $ do
        form_ [method_ "post", action_ "/final/submit/arith"] $ do
            label_ [for_ "user-input"] "Arithmetic"
            textarea_ [id_ "user-input", name_ "user-input", style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        form_ [method_ "post", action_ "/final/submit/logic"] $ do
            label_ [for_ "user-input"] "Logic"
            textarea_ [id_ "user-input", name_ "user-input", style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
finalPage res =
    base "Final Playground" $ do
        form_ [method_ "post", action_ "/final/submit/arith"] $ do
            label_ [for_ "user-input"] "Arithmetic"
            textarea_ [id_ "user-input", name_ "user-input", style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        form_ [method_ "post", action_ "/final/submit/logic"] $ do
            label_ [for_ "user-input"] "Logic"
            textarea_ [id_ "user-input", name_ "user-input", style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        div_ [class_ "ui clearing divider"] ""
        h2_ [] "Output"
        toHtml res
