{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import qualified Data.List.NonEmpty as NE
import Options.Applicative

import Sylvia.Renderer.Impl.Cairo
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
    case mapM parseExp $ NE.toList inputs of
        Left derp -> print derp
        Right es -> case outputFile of
            Nothing -> showInWindow es
            Just filename
                -> writePNG filename . stackHorizontally $ map render es

data Sylvia = Sylvia
    { outputFile :: Maybe FilePath
    , inputs :: NE.NonEmpty String
    }
  deriving (Show)

sylvia :: Parser Sylvia
sylvia = Sylvia
    <$> option
        ( long "output"
        & short 'o'
        & metavar "FILENAME"
        & help "Write the result to a PNG image"
        & value Nothing
        & reader (\s -> if null s then Nothing else Just (Just s))
        )
    <*> expressions
  where
    expressions = (NE.:|)
        <$> argument Just
            ( metavar "EXPR"
            & help "Expressions to render, in zero-based De Bruijn index notation"
            )
        <*> arguments Just
            ( metavar "EXPR" )
