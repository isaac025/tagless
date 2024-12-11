{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module FinalPage where

import Lucid
import Pages
import Pet (Pet (..))

finalPage :: Pet -> Html ()
finalPage Pet{..} =
    base "Final Playground" $ do
        form_ [class_ "ui form", method_ "post", action_ "/final/submit"] $ do
            div_ [class_ "field"] $ do
                label_ [for_ "user-input"] "Final Pet Maker"
                input_ [type_ "text", id_ "user-input", name_ "user-input"]
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        div_ [class_ "ui clearing divider"] ""
        p_ [] ("My name is: " <> toHtml petName)
        img
  where
    img =
        if
            | petState == "sleep" -> img_ [style_ "width: 200px", src_ "/static/imgs/sleeping-in-bed-1.gif"]
            | petState == "eat" -> img_ [style_ "width: 200px", src_ "/static/imgs/eat-fish.gif"]
            | petState == "bathe" -> img_ [style_ "width: 200px", src_ "/static/imgs/take-a-bath.gif"]
            | otherwise -> img_ [style_ "width: 200px", src_ "/static/imgs/dog-understand.gif"]
