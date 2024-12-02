{-# LANGUAGE ImportQualifiedPost #-}

module Pages where

import Data.Functor.Identity
import Data.Text (Text)
import Lucid

base :: HtmlT Identity () -> Html () -> Html ()
base t h =
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                title_ "CCOM 6029 Presentation"
                link_ [rel_ "stylesheet", href_ "/static/semantic.min.css"]
                style_ "body { display: flex; justify-content: center; align-items: center; height: 100vh; background-color: #f7f7f7; }"
            body_ $ do
                div_ [class_ "ui raised very padded text container segment"] $ do
                    h1_ [class_ "ui floated header"] t
                    div_ [class_ "ui clearing divider"] ""
                    h

nextButton :: Text -> Html ()
nextButton endpoint =
    a_ [href_ $ "http://localhost:8080/" <> endpoint, class_ "primary ui right labeled icon button"] $ do
        i_ [class_ "right arrow icon"] ""
        "Next"

introPage :: Html ()
introPage =
    base "Typed Tagless Final Interpreters" $ do
        i_ [] "Isaac H. Lopez Diaz"
        ul_ [] $ do
            li_ [] "Original: Finally Tagless, Partially Evaluated Tagless Staged Interpreters for Simpler Typed Languages"
            li_ [] "Titulo alterno: An alternative approach to building Embedded DSLs"
        nextButton "what-is-it-about"

backgrounPage :: Html ()
backgrounPage =
    base "De que habla el articulo?" $ do
        ul_ [] $ do
            li_ $ do
                "Initial Encoding - Usar ADT para implementar el interpretador"
                pre_ [] $ do
                    code_ "data Expr = Int | Plus Expr Expr"
            li_ [] $ do
                "Final Encoding - Usar typeclasses mejor"
                pre_ [] $ do
                    code_ [] "class Expr repr where"
                pre_ [] $ do
                    code_ [] "int :: Int -> repr"
                pre_ [] $ do
                    code_ [] "plus :: repr -> repr -> repr"
        nextButton "lesson"

lessonPage :: Html ()
lessonPage =
    base "Lecciones" $ do
        ul_ [] $ do
            li_ [] "Mejor forma de escribir Embedded DSLs con tipos"
            li_ [] $ do
                "Interpretadores y DSLs mas ampliables ("
                i_ "more extensible"
                ")"
        nextButton "playground"

playgroundPage :: String -> Html ()
playgroundPage "" =
    base "Playground" $ do
        form_ [method_ "post", action_ "/playground/initial"] $ do
            label_ [for_ "initial"] "Initial"
            textarea_ [id_ "initial", name_ "initial", style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        form_ [method_ "post", action_ "/playground"] $ do
            label_ [] "Final"
            textarea_ [style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
playgroundPage ast =
    base "Playground" $ do
        form_ [method_ "post", action_ "/playground/initial"] $ do
            label_ [for_ "initial"] "Initial"
            textarea_ [id_ "initial", name_ "initial", style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        form_ [method_ "post", action_ "/playground"] $ do
            label_ [] "Final"
            textarea_ [style_ "width: 620px; height: 120px"] ""
            button_ [class_ "positive ui button", type_ "submit"] "Interpret"
        div_ [class_ "ui clearing divider"] ""
        h2_ [] "Output"
        toHtml ast
