{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import Options.Applicative

import Sylvia.Render.Core
import Sylvia.Render.Backend.Cairo
import Sylvia.Text.Parser
import Sylvia.UI.GTK

main :: IO ()
main = execParser opts >>= process
  where
    opts = info (helper <*> sylvia)
        ( fullDesc
        & progDesc "Render a lambda expression"
        & header "sylvia - a lambda calculus visualizer"
        )

process :: Sylvia -> IO ()
process (Sylvia{..}) = do
    case parseExp input of
        Left derp -> print derp
        Right e -> case outputFile of
            Nothing -> showInWindow [e]
            Just filename -> writePNG filename $ render e

data Sylvia = Sylvia
    { outputFile :: Maybe FilePath
    , input :: String
    }
  deriving (Show)

sylvia :: Parser Sylvia
sylvia = Sylvia
    <$> nullOption
        ( long "output"
        & short 'o'
        & metavar "FILENAME"
        & help "Write the result to a PNG image"
        & value Nothing
        & reader (Just . Just)
        )
    <*> argument Just
        ( metavar "EXPR"
        & help "Expression to render, in zero-based De Bruijn index notation"
        )
