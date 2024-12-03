module Main where

import Server

{-repl :: IO ()
repl = do
    putStr "> "
    l <- getLine
    when (l == "q" || l == "quit") exitSuccess
    let parsed = parser l
    case parsed of
        Left err -> print err
        Right ast -> print ast
    repl
-}

main :: IO ()
main =
    {- do
    hSetBuffering stdout NoBuffering
    repl -} runApp
