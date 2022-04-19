module Main where

import Lib

import System.IO
import System.Exit
import System.Environment ( getArgs )
import System.Random ( StdGen, getStdRandom, randomR )

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Int ( Int8 )
import Text.Read ( readMaybe )

type Point = (Int, Int)
type Color = (Int8, Int8, Int8)

data Pixel = Pixel {
    getPos :: Point,
    getColor :: Color
} deriving (Show)

data Opts = Opts {
    getColors :: Int,
    getLimit :: Float,
    getPath :: String
}

getRed   :: Color -> Int8
getGreen :: Color -> Int8
getBlue  :: Color -> Int8
getRed   (r,_,_) = r
getGreen (_,g,_) = g
getBlue  (_,_,b) = b

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

removeAt :: [a] -> Int -> [a]
removeAt list i = (take i list) ++ (drop (i + 1) list)

chooseRandom :: Int -> [a] -> IO [a]
chooseRandom 0 _ = return []
chooseRandom n list = do
    randIndex <- getStdRandom $ randomR (0, (length list) - 1)
    points <- chooseRandom (n - 1) (removeAt list randIndex)
    return ((list !! randIndex):points)

colorDist :: Color -> Color -> Float
colorDist start end =
    let r = fromIntegral((getRed end) - (getRed start))
        g = fromIntegral((getGreen end) - (getGreen start))
        b = fromIntegral((getBlue end) - (getBlue start))
    in r ** 2 + g ** 2 + b ** 2

printPixels :: [Pixel] -> IO ()
printPixels [] = return ()
printPixels (pixel:xs) = do
    putStrLn (show pixel)
    printPixels xs

main :: IO ()
main = do
    opts <- execParser optsParser
    file <- readFile (getPath opts)
    let img = parseImg (lines file)
    pixels <- chooseRandom (getColors opts) img
    putStrLn $ show $ (colorDist (getColor $ (pixels !! 0)) (getColor $ (pixels !! 1)))