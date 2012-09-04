{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import Options.Applicative

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
            Nothing -> showInWindow e
            Just filename -> putStrLn "Insert code here"

data Sylvia = Sylvia
    { outputFile :: Maybe FilePath
    , input :: String
    }
  deriving (Show)

sylvia :: Parser Sylvia
sylvia = Sylvia
    <$> maybeP $: strOption
        ( long "output"
        & short 'o'
        & metavar "FILENAME"
        & help "Write the result to a PNG image"
        )
    <*> argument Just
        ( metavar "EXPRESSION"
        & help "Expression to render, in zero-based De Bruijn index notation"
        )

-- | Modify the parser so it returns Nothing instead of blowing up.
maybeP :: Parser a -> Parser (Maybe a)
maybeP p = (Just <$> p) <|> pure Nothing

-- | A more tightly binding version of '$'.
($:) :: (a -> b) -> a -> b
($:) = ($)
infixr 9 $:
