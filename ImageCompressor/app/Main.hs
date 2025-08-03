{-
-- EPITECH PROJECT, 2025
-- compressor
-- File description:
-- Main
-}

module Main (main) where
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))
import Data.Char (isDigit)
import Lib (compressorMain)
import Text.Read (readMaybe)
import System.IO.Error(tryIOError)

-- Function to get the count of a specific character in a string
charCount :: Char -> String -> Int
charCount c str = length [x | x <- str, x == c]

-- Function to read a file, returning either the contents or an error message
tryReadFile :: FilePath -> IO (Either String String)
tryReadFile fileName = do
    fileContents <- tryIOError (readFile fileName)
    return $ case fileContents of
        Left err -> Left (show err)
        Right content -> Right content

-- Removes everything but numbers to check for bad characters
keepNumbers :: String -> String
keepNumbers = filter (`notElem` "(, )")

-- Split a string by a delimiter
splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delim str =
    let (before, remainder) = break (== delim) str in before : case remainder of
    [] -> []
    x -> splitBy delim (drop 1 x)

-- Check if string can be a float
isValidFloat :: String -> Bool
isValidFloat str = case (readMaybe str :: Maybe Float) of
    Just _  -> True
    Nothing -> False

-- Checks every argument passed in ARGVs and see if each is valid
validArgs :: [String] -> IO()
validArgs [] = return ()
validArgs ("-n":y:xs)
    | all isDigit y = validArgs xs
    | otherwise =
        putStrLn "Error, setting for -n is not valid" >>
        exitWith (ExitFailure 84)
validArgs ("-l":y:xs)
    | isValidFloat y = validArgs xs
    | otherwise =
        putStrLn "Error, setting for -l is not valid" >>
        exitWith (ExitFailure 84)
validArgs ("-f":_:xs) = validArgs xs
validArgs _ =
    putStrLn "Error, unknown setting" >>
    exitWith (ExitFailure 84)

-- Counts the number of arguments (error handling)
countArgs :: Int -> [String] -> IO ()
countArgs 0 [] =
    putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n" >>
    putStrLn "    N    number of colors in the final image" >>
    putStrLn "    L    convergence limit" >>
    putStrLn "    F    path to the file containing the colors of the pixels" >>
    exitWith (ExitFailure 84)
countArgs 6 [] = return ()
countArgs _ [] =
        putStrLn "Error, invalid arguments" >>
        exitWith (ExitFailure 84)
countArgs n (_:xs) = countArgs (n + 1) xs

-- Reads the file and checks if there is no bad characters and somewhat good formatting
properFile :: [String] -> Int -> IO ()
properFile [] _ = return ()
properFile str pos
    | all isDigit (keepNumbers (str !! 0)) && charCount ',' (str !! 0) == 3
    && charCount '(' (str !! 0) == 2 && charCount ')' (str !! 0) == 2 &&
    charCount ' ' (str !! 0) == 1 = properFile (drop 1 str) (pos + 1)
    | otherwise =
        putStrLn ("Error, file is invalid\n(error found at line " ++
        show pos ++ ": " ++ str !! 0 ++ ")") >>
        exitWith (ExitFailure 84)

errorHandling :: [String] -> IO ()
errorHandling args = do
    countArgs 0 args
    validArgs args
    contents <- tryReadFile (args !! 5)
    case contents of
        Left err -> putStrLn ("Error: "++show err) >> exitWith(ExitFailure 84)
        Right fileContent -> properFile (splitBy '\n' fileContent) 0

main :: IO ()
main = do
    args <- getArgs
    errorHandling args
    compressorMain (read (args !! 1)) (read (args !! 3)) (args !! 5)
    exitSuccess
