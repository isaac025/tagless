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
    a_ [href_ $ "http://ihld.xyz:8080/" <> endpoint, class_ "primary ui right labeled icon button"] $ do
        i_ [class_ "right arrow icon"] ""
        "Next"

introPage :: Html ()
introPage =
    base "Typed Tagless Final Interpreters" $ do
        div_ [] $ i_ [] "Isaac H. López Díaz"
        div_ [] $ i_ [] "CCOM 6029"
        div_ [] $ i_ [] "Humberto Ortíz-Zauzaga"
        ul_ [] $ do
            li_ [] "Original: Finally Tagless, Partially Evaluated Tagless Staged Interpreters for Simpler Typed Languages"
            li_ [] "Título alterno: An alternative approach to building Embedded DSLs"
        nextButton "what-is-it-about"

backgroundPage :: Html ()
backgroundPage =
    base "¿De qué habla el artículo?" $ do
        img_ [alt_ "rice-beans", src_ "/static/imgs/arroz.jpg", style_ "width: 200px"]
        ul_ [] $ do
            li_ $ do
                "Initial Encoding - Usar ADT para implementar el interpretador"
                pre_ [] $ do
                    code_ "data Expr = IntE Int | Plus Expr Expr"
                pre_ [] $ do
                    code_ "data Expr = IntE Int | BoolE Bool | Plus Expr Expr"
            li_ [] $ do
                "Final Encoding - Usar typeclasses mejor"
                pre_ [] $ do
                    code_ [] "class Expr expr where"
                pre_ [] $ do
                    code_ [] "    lit :: a -> expr"
                pre_ [] $ do
                    code_ [] "    plus :: expr -> expr -> expr"
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
        nextButton "initial"
