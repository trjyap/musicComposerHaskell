module Main where

import MusicData
import MusicFunctions
import System.IO
import System.Exit (exitSuccess)

-- Loops the main menu
menu :: IO ()
menu = do
    putStrLn "\n=== Melody Composer Menu ==="
    putStrLn "1. Create a new melody"
    putStrLn "2. Save melody to file"
    putStrLn "3. Load melody from file"
    putStrLn "4. Exit"
    putStr "Choose an option: "
    hFlush stdout -- Ensures the prompt displays before input
    option <- getLine
    handleOption option Nothing

-- Handles the user menu options
handleOption :: String -> Maybe Melody -> IO ()
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
    menuLoop melody
handleOption "4" _ = do
    putStrLn "Exiting... Goodbye!"
    exitSuccess
handleOption _ melody = do
    putStrLn "Invalid option! Please try again."
    menuLoop melody

-- Loops back to menu with current melody state
menuLoop :: Maybe Melody -> IO ()
menuLoop melody = menu >> handleOption "" melody

-- Main function
main :: IO ()
main = do
    putStrLn "Welcome to the Haskell Melody Composer!"
    menu
