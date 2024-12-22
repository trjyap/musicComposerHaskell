module Main where

import MusicData
import MusicFunctions
import System.IO
import System.Exit (exitSuccess)

-- Loops back to menu with current melody state
menuLoop :: Maybe (Melody MusicElement) -> IO ()
menuLoop melody = 
    putStrLn "\n=== Melody Composer Menu ===" >>
    putStrLn "1. Create a new melody" >>
    putStrLn "2. Save melody to file" >>
    putStrLn "3. Load melody from file" >>
    putStrLn "4. Transpose current melody" >>
    putStrLn "5. Exit" >>
    putStr "Choose an option: " >>
    hFlush stdout >> -- Ensures the prompt displays before input
    getLine >>= \option -> handleOption option melody

-- Handles the user menu options
handleOption :: String -> Maybe (Melody MusicElement) -> IO ()
handleOption "1" _ = do
    melody <- createMelody
    menuLoop (Just melody) -- Passes the created melody to the next loop
handleOption "2" (Just melody) = do
    putStr "Enter filename to save: "
    hFlush stdout
    filePath <- getLine
    saveMelody filePath melody
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
    putStrLn "No melody to transpose! Please load a melody first."
    menuLoop Nothing
handleOption "5" _ = do
    putStrLn "Exiting... Goodbye!\n"
    exitSuccess
handleOption _ melody = do
    putStrLn "Invalid option! Please try again."
    menuLoop melody

-- Main function
main :: IO ()
main = do
    putStrLn "Welcome to the Haskell Melody Composer!"
    menuLoop Nothing
