module Main
    ( main
    ) where

import Control.Monad ( forever )
import System.IO ( hPutStr, stderr )

import Sylvia.Text.Parser
import Sylvia.UI.GTK

main :: IO ()
main = forever $ do
    hPutStr stderr "sylvia> "
    input <- getLine
    case parseExp input of
        Left  err -> putStrLn $ show err
        Right res -> showInWindow res
