module MusicFunctions (createMelody, saveMelody, loadMelody) where

import MusicData
import System.IO
import System.Directory (doesFileExist)
import Data.List (intercalate)

-- Prompts the user to create a new melody
createMelody :: IO Melody
createMelody = do
    putStrLn "Enter notes for your melody (comma-separated, e.g., C4,D4,E4):"
    input <- getLine
    let melody = wordsWhen (== ',') input
    putStrLn "Melody created!"
    return melody

-- Saves the melody to a specified file
saveMelody :: FilePath -> Melody -> IO ()
saveMelody filePath melody = do
    writeFile filePath (intercalate "," melody)
    putStrLn $ "Melody saved to " ++ filePath

-- Loads a melody from a specified file
loadMelody :: FilePath -> IO (Maybe Melody)
loadMelody filePath = do
    exists <- doesFileExist filePath
    if exists
        then do
            content <- readFile filePath
            let melody = wordsWhen (== ',') content
            putStrLn "Melody loaded successfully!"
            return (Just melody)
        else do
            putStrLn "File not found!"
            return Nothing
