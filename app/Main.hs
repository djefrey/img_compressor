module Main where

import Lib

import System.IO
import System.Exit
import System.Environment ( getArgs )
import System.Random ( StdGen, getStdRandom, randomR )

import Options.Applicative
import Data.Semigroup ((<>))
import Data.List ( elemIndex )
import Text.Read ( readMaybe )

import Control.Exception ( try, IOException )

type Point = (Int, Int)
type Color = (Int, Int, Int)

data Opts = Opts {
    oGetColors :: Int,
    oGetLimit :: Float,
    oGetPath :: String
}

data Pixel = Pixel {
    pGetPos :: Point,
    pGetColor :: Color
}
instance Show Pixel where
    show = showPixel

data Cluster = Cluster {
    cGetColor :: Color,
    cGetPixels :: [Pixel]
}
instance Show Cluster where
    show = showCluster

getRed   :: (a,b,c) -> a
getGreen :: (a,b,c) -> b
getBlue  :: (a,b,c) -> c
getRed   (r,_,_) = r
getGreen (_,g,_) = g
getBlue  (_,_,b) = b

optsParser :: ParserInfo Opts
optsParser = info (options) (fullDesc)
    where
    options :: Parser Opts
    options = Opts
            <$> option auto (short 'n' <> metavar "N"
                <> help "number of colors in the final image")
            <*> option auto (short 'l' <> metavar "L"
                <> help "convergence limit")
            <*> strOption (short 'f' <> metavar "F"
                <> help "path to the file containing the colors of the pixels")

replaceCommas :: Char -> Char
replaceCommas ',' = ' '
replaceCommas c = c

parseEntry :: [String] -> Pixel
parseEntry (x:y:r:g:b:[]) =
    (Pixel (read x :: Int, read y :: Int)
    (read r :: Int, read g :: Int, read b :: Int))

lineToPixel :: String -> Pixel
lineToPixel line =
    let values = words (map replaceCommas (filter (`notElem` "()") line))
    in parseEntry values

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

addToCluster :: Pixel -> Cluster -> Cluster
addToCluster pixel (Cluster color pixels) = (Cluster color (pixel:pixels))

addToClosestCluster :: Pixel -> [Cluster] -> [Cluster]
addToClosestCluster pixel clusters =
    let dists = map (\c -> colorDist (cGetColor c) (pGetColor pixel)) clusters
        (Just index) = elemIndex (minimum dists) dists
        cluster = clusters !! index
    in (take index clusters) ++ [(addToCluster pixel cluster)]
    ++ (drop (index + 1) clusters)

findClosestClusters :: [Pixel] -> [Cluster] -> [Cluster]
findClosestClusters [] clusters = clusters
findClosestClusters (p:ps) clusters =
    let updatedClusters = addToClosestCluster p clusters
    in findClosestClusters ps updatedClusters

sumColor :: Color -> Color -> Color
sumColor (r1,g1,b1) (r2,g2,b2) = ((r1 + r2),(g1 + g2),(b1 + b2))

divColor :: Color -> Int -> Color
divColor (r,g,b) n = ((div r n),(div g n),(div b n))

moveClusterCentroid :: Cluster -> Cluster
moveClusterCentroid (Cluster _ pixels) =
    let sum = foldr sumColor (0,0,0) (map pGetColor pixels)
    in (Cluster (divColor sum (length pixels)) pixels)

getBiggestClusterMove :: [Cluster] -> [Cluster] -> Float
getBiggestClusterMove old new =
    let pairs = zip (map cGetColor old) (map cGetColor new)
    in maximum $ map (\(o,n) -> colorDist o n) pairs

printData :: Show a => [a] -> IO ()
printData [] = return ()
printData (x:xs) =
    (putStrLn $ show x)
    >> printData xs

showPixel :: Pixel -> String
showPixel (Pixel pos color) = (show pos) ++ " " ++ (show color)

showCluster :: Cluster -> String
showCluster (Cluster color pixels) =
    let pxList = foldr (++) "" (map (\p -> (show p) ++ "\n") pixels)
        str = "--\n" ++ (show color) ++ "\n-\n" ++ pxList
    in take ((length str) - 1) str

safeReadFile :: String -> IO String
safeReadFile path = do
    fileExecpt <- try $ readFile path
    case (fileExecpt :: Either IOException String) of
        Left _ -> exitWith (ExitFailure 84)
        Right contents -> return contents

checkPixelsValues :: [Pixel] -> Bool
checkPixelsValues [] = True
checkPixelsValues ((Pixel (x,y) (r,g,b)):ps) =
    ((length (filter (\i -> i < 0) [x,y])) == 0)
    && ((length (filter (\i -> i < 0 || i > 255 ) [r,g,b])) == 0)
    && (checkPixelsValues ps)

run :: [Pixel] -> Opts -> IO ()
run img opts = do
    colors <- chooseRandom (oGetColors opts) (map (\p -> pGetColor p) img)
    let clusters = kmeans img (map createClusters colors) (oGetLimit opts)
    printData clusters
    where
        createClusters :: Color -> Cluster
        createClusters color = (Cluster color [])

kmeans :: [Pixel] -> [Cluster] -> Float -> [Cluster]
kmeans img clusters limit =
    let pxUpdatedClusters = findClosestClusters img clusters
        posUpdatedClusters = map moveClusterCentroid pxUpdatedClusters
    in if getBiggestClusterMove clusters posUpdatedClusters >= limit ** 2
        then kmeans img (map (\(Cluster color _)
            -> (Cluster color [])) posUpdatedClusters) limit
        else posUpdatedClusters

main :: IO ()
main = do
    opts <- execParser optsParser
    file <- safeReadFile (oGetPath opts)
    let img = parseImg (lines file)
    case checkPixelsValues img of
        True -> run img opts
        False -> exitWith (ExitFailure 84)
