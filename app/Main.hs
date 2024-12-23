module Main where

import MusicData
import MusicFunctions
import System.IO
import System.Exit (exitSuccess)
import System.Directory (createDirectoryIfMissing, doesFileExist)

-- Loops back to menu with current melody state
menuLoop :: Maybe (Melody MusicElement) -> IO ()
menuLoop melody = 
    putStrLn "\n=== Melody Composer Menu ===" >>
    putStrLn "1. Create a new melody" >>
    putStrLn "2. Save melody to file" >>
    putStrLn "3. Load melody from file" >>
    putStrLn "4. Transpose current melody" >>
    putStrLn "5. Combine melodies" >>
    putStrLn "6. Exit" >>
    putStr "Choose an option: " >>
    hFlush stdout >> -- Ensures the prompt displays before input
    getLine >>= \option -> handleOption option melody -- Passes the input to handelOption function

-- Handles the user menu options
handleOption :: String -> Maybe (Melody MusicElement) -> IO ()
handleOption "1" _ = do
    melody <- createMelody
    play melody -- Shows the created melody to the user
    menuLoop (Just melody) -- Passes the created melody to the next loop
handleOption "2" (Just melody) = do
    let outputFilePath = "output/"  -- Ensures outputs go to the same folder
    createDirectoryIfMissing True outputFilePath
    putStr "Enter filename to save: "
    hFlush stdout
    fileName <- getLine
    let filePath = outputFilePath ++ fileName   -- Combines path and filename
    fileExists <- doesFileExist filePath
    if fileExists 
        then do
            putStrLn "File already exists! Overwrite? (y/n)"
            overwrite <- getLine
            if overwrite == "y"
                then saveMelody filePath melody
                else putStrLn "File not saved."
        else saveMelody filePath melody
    menuLoop (Just melody)
handleOption "2" Nothing = do
    putStrLn "No melody to save! Please create a melody first."
    menuLoop Nothing
handleOption "3" _ = do
    putStr "Enter filename to load: "
    hFlush stdout
    filePath <- getLine
    melody <- loadMelody filePath
    case melody of
        Just m -> play m -- Print out loaded melody
        Nothing -> putStrLn "Failed to load melody."
    menuLoop melody
handleOption "4" (Just melody) = do
    putStr "Enter the number of semitones to transpose: "
    hFlush stdout
    semitones <- getLine
    putStrLn "\nTransposing melody..."
    let transposedMelody = transposeMelody (read semitones) melody
    play transposedMelody
    menuLoop (Just transposedMelody)
handleOption "4" Nothing = do
    putStrLn "No melody to transpose! Please load or create a melody first."
    menuLoop Nothing
handleOption "5" (Just melody) = do
    putStrLn "Enter the filename of the first melody to combine with:"
    hFlush stdout
    filePath <- getLine
    melody1 <- loadMelody filePath
    case melody1 of
        Just m -> do 
            let combinedMelody = combineMelodies m melody
            putStrLn "Melodies combined successfully!"
            play combinedMelody
            menuLoop (Just combinedMelody)
        Nothing -> do
            putStrLn "Failed to load the second melody."
            menuLoop (Just melody)
handleOption "5" Nothing = do    -- Empty melody state
    putStrLn "No melody to combine with! Please create or load a melody first."
    menuLoop Nothing
handleOption "6" _ = do 
    putStrLn "Exiting... Goodbye!\n"
    exitSuccess
handleOption _ melody = do
    putStrLn "Invalid option! Please try again."
    menuLoop melody

-- Main function
main :: IO ()
main = do
    runQuickCheckTests
    putStrLn "\nWelcome to the Haskell Melody Composer!"
    menuLoop Nothing
