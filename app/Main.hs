module Main where

import Lib

import System.IO
import System.Exit
import System.Environment ( getArgs )

import Options.Applicative
import Data.Semigroup ((<>))

data Opts = Opts {
    colors :: Int,
    limit :: Float,
    path :: String
}

optsParser :: ParserInfo Opts
optsParser = info (options) (fullDesc)
    where
    options :: Parser Opts
    options = Opts
            <$> option auto (
                short 'n'
                <> metavar "N"
                <> help "number of colors in the final image")
            <*> option auto (
                short 'l'
                <> metavar "L"
                <> help "convergence limit")
            <*> strOption (
                short 'f'
                <> metavar "F"
                <> help "path to the file containing the colors of the pixels")

main :: IO ()
main = do
    opts <- execParser optsParser
    return ()