module Main where

import Lib

import System.IO
import System.Exit
import System.Environment ( getArgs )

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Int ( Int8 )
import Text.Read ( readMaybe )

data Pixel = Pixel {
    getPos :: (Int, Int),
    getColor :: (Int8, Int8, Int8)
} deriving (Show)

data Opts = Opts {
    getColors :: Int,
    getLimit :: Float,
    getPath :: String
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

lineToPixel :: String -> Pixel
lineToPixel line =
    let values = words (map replaceCommas (filter (`notElem` "()") line))
    in parseEntry values
    where
        replaceCommas :: Char -> Char
        replaceCommas c = case c of
            ',' -> ' '
            _ -> c
        parseEntry :: [String] -> Pixel
        parseEntry (x:y:r:g:b:[]) =
            (Pixel (read x :: Int, read y :: Int)
            (read r :: Int8, read g :: Int8, read b :: Int8))

parseImg :: [String] -> [Pixel]
parseImg [] = []
parseImg (line:xs) = (lineToPixel line):(parseImg xs)

printPixels :: [Pixel] -> IO ()
printPixels [] = return ()
printPixels (pixel:xs) = do
    putStrLn (show pixel)
    printPixels xs

main :: IO ()
main = do
    opts <- execParser optsParser
    imgFile <- readFile (getPath opts)
    printPixels (parseImg (lines imgFile))