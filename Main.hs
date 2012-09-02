module Main
    ( main
    ) where

import System.Environment ( getArgs )

import Sylvia.Text.Parser
import Sylvia.UI.GTK

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input] -> case parseExp input of
            Left  err -> putStrLn $ show err
            Right res -> showInWindow res
        _ -> putStrLn "Usage: sylvia 'expression'"
