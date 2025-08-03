{-
-- EPITECH PROJECT, 2025
-- compressor
-- File description:
-- Lib
-}

module Lib (compressorMain) where
import System.Random (randomRIO)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Control.Monad (replicateM)
import Data.List (nub)

-- Types de variables
type Position = (Int, Int)
type Color = (Int, Int, Int)
type Pixel = (Position, Color)
type Centroid = Color
type Cluster = (Centroid, [Pixel])

-- Fonction carré car flemme les warnings
squared :: Int -> Int
squared x = x * x

-- Distance euclidienne entre deux couleurs
colorDistance :: Color -> Color -> Double
colorDistance (a1, a2, a3) (b1, b2, b3) =
    sqrt (fromIntegral (squared (a1-b1) + squared (a2-b2) + squared (a3-b3)))

-- Trouve le centroide le plus proche pour un pixel
nearestCentroid :: [Centroid] -> Pixel -> Centroid
nearestCentroid centroids pixel =
    fst (minimumBy (comparing snd) 
        [(c, colorDistance c (snd pixel)) | c <- centroids])

-- Calcule la couleur moyenne d'un groupe de pixels
averageColor :: [Pixel] -> Color
averageColor [] = (0, 0, 0)
averageColor pixels =
    let (rs, gs, bs) = unzip3 (map snd pixels)
        n = length pixels
        avg xs = sum xs `div` n
    in (avg rs, avg gs, avg bs)

-- Assigne les pixels aux centroides les plus proches
assignPixels :: [Centroid] -> [Pixel] -> [Cluster]
assignPixels centroids pixels =
    let -- Groupe les pixel au centroide les plus proches
        findCluster pixel = (nearestCentroid centroids pixel, pixel)
        groupedPixels = map findCluster pixels
        -- Organise les pixels en clusters suivant leur centroides
        buildCluster centroid =
            (centroid, [p | (c, p) <- groupedPixels, c == centroid])
    in map buildCluster centroids

-- Mise à jour des centroides en faisant la moyenne de chaque cluster
updateCentroids :: [Cluster] -> [Centroid]
updateCentroids clusters = map (averageColor . snd) clusters

-- Vérifie la convergence des centroides
hasConverged :: Double -> [Centroid] -> [Centroid] -> Bool
hasConverged limit oldCentroids newCentroids =
    all (< limit)
    [colorDistance old new | (old, new) <- zip oldCentroids newCentroids]

-- Function to check for duplicates in an array of integers
hasDuplicates :: [Int] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

-- Initialisation de k centroides aléatoires
initializeCentroids :: Int -> [Pixel] -> IO [Centroid]
initializeCentroids k pixels =
    if (k <= length pixels)
        then do
        indices <- replicateM k (randomRIO (0, length pixels - 1))
        if ((hasDuplicates indices))
            then initializeCentroids k pixels
            else return (map (snd . (pixels !!)) indices)
        else exitWith(ExitFailure 84)

-- Algorithme K-means principal
kMeans :: Double -> Int -> [Pixel] -> IO [Cluster]
kMeans limit k pixels = do
    initialCentroids <- initializeCentroids k pixels
    let iterateClusters centroids
            | hasConverged limit centroids newCentroids = return clusters
            | otherwise = iterateClusters newCentroids
            where
            clusters = assignPixels centroids pixels
            newCentroids = updateCentroids clusters
    iterateClusters initialCentroids

-- Parsing du fichier
parseInput :: String -> [Pixel]
parseInput input = mapMaybe parseLine (lines input)
    where
        parseLine line = case words line of
            [pos, color] -> do
                (x, y) <- readMaybe pos
                (r, g, b) <- readMaybe color
                return ((x, y), (r, g, b))
            _ -> Nothing

-- Formattage pour l'affichage
formatOutput :: [Cluster] -> String
formatOutput clusters = concatMap formatCluster clusters where
    formatCluster (centroid, pixels) = 
        "--\n" ++ 
        formatColor centroid ++ "\n-\n" ++
        concatMap formatPixel pixels
    formatColor (r,g,b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++")"
    formatPixel ((x,y), (r,g,b)) = 
        "(" ++ show x ++ "," ++ show y ++ ") " ++ formatColor (r,g,b) ++ "\n"

-- Fonction principale
compressorMain :: Int -> Double -> FilePath -> IO ()
compressorMain numColors convergenceLimit inputFile = do
    input <- readFile inputFile
    if null (parseInput input) 
        then
            hPutStrLn stderr "Error: Empty input file or parsing failed" >>
            exitWith (ExitFailure 84)
        else do
            clusters <- kMeans convergenceLimit numColors (parseInput input)
            putStr (formatOutput clusters)