{-# LANGUAGE FlexibleInstances #-}
module MusicFunctions where

import MusicData
import System.Directory (doesFileExist)
import Data.Maybe (isJust)
import Data.Ratio (numerator, denominator, (%))
import Test.QuickCheck (Arbitrary, arbitrary, suchThat)

-- Utility function to split a string by a delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

-- Converts a string to a Note
stringToNote :: String -> Either String Note
stringToNote str = case wordsWhen (== ',') str of
    [pitchStr, durStr] ->
        case (reads pitchStr, reads durStr) of
            ([(p, "")], [(d, "")]) -> Right (Note p d)
            _                     -> Left "Invalid pitch or duration format. Ensure it is in the form 'pitch,duration' (e.g., '60,1%4')."
    _ -> Left "Invalid note format. Use 'pitch,duration' (e.g., '60,1%4')."

-- Converts a string to a MusicElement
stringToMusicElement :: String -> Either String MusicElement
stringToMusicElement str =
    case wordsWhen (== '|') str of
        [elementStr] -> parseElement elementStr
        _            -> Left "Invalid format. Use '|' to separate elements (notes or chords)."
  where
    parseElement :: String -> Either String MusicElement
    parseElement elementStr =
        -- Try parsing as a single note (pitch,duration)
        case wordsWhen (== ',') elementStr of
            [pitchStr, durStr] -> 
                case (reads pitchStr, reads durStr) of
                    ([(p, "")], [(d, "")]) -> Right (SingleNote (Note p d))
                    _ -> Left "Invalid pitch or duration format for note."
            _ -> 
                -- Try parsing as a chord: multiple notes separated by spaces
                let noteStrings = words elementStr
                in if length noteStrings > 1
                    then 
                        let notes = map stringToNote noteStrings
                        in case sequence notes of
                            Left err -> Left $ "Invalid chord format. " ++ err
                            Right validNotes -> Right (ChordElement (Chord validNotes))
                    else Left "Invalid input format. Please check for commas and spaces."

-- Converts a string to a Melody
stringToMelody :: String -> Either String (Melody MusicElement)
stringToMelody str =
    let elements = map stringToMusicElement (wordsWhen (== '|') str)
    in case sequence elements of
        Left err -> Left $ "Invalid melody format: " ++ err
        Right validElements -> Right (Melody validElements)

-- Creates a note
createNote :: Pitch -> Duration -> Note
createNote p d = Note { pitch = p, duration = d }

-- Prompts the user to create a new melody
createMelody :: IO (Melody MusicElement)
createMelody = do
    putStrLn "Enter notes and chords for your melody (separate with \"|\"). For chords, separate notes with a space (e.g., 60,1%4 64,1%4 67,1%4): "
    input <- getLine
    let elementStrings = wordsWhen (== '|') input -- Pipe-separated notes and chords
    let elements = map stringToMusicElement elementStrings
    case sequence elements of
        Left err -> do
            putStrLn $ "Error: " ++ err
            createMelody -- Retry on error
        Right validElements -> do
            putStrLn "Melody created successfully!"
            return $ Melody validElements


-- Converts a single Note to a String format "pitch,duration"
noteToString :: MusicElement -> String
noteToString (SingleNote (Note p d)) = show p ++ "," ++ durationToString d
noteToString (ChordElement (Chord notes)) = unwords (map (noteToString . SingleNote) notes)

-- Helper function to convert Duration to String without spaces
durationToString :: Duration -> String
durationToString d = let (n, d') = (numerator d, denominator d)
                     in show n ++ "%" ++ show d'

-- Saves the melody to a specified file
saveMelody :: FilePath -> Melody MusicElement -> IO ()
saveMelody filePath (Melody elements) = do
    let content = unlines $ map noteToString elements
    writeFile filePath content
    putStrLn $ "Melody saved to " ++ filePath

-- Parses a single line into a Note
parseMusicElement :: String -> Maybe MusicElement
parseMusicElement line =
    case stringToMusicElement line of
        Right element -> Just element
        Left _ -> Nothing

-- Loads a melody from a specified file
loadMelody :: FilePath -> IO (Maybe (Melody MusicElement))
loadMelody filePath = do
    exists <- doesFileExist filePath
    if exists
        then do
            content <- readFile filePath
            let elements = map parseMusicElement (lines content)
            if all isJust elements
                then do
                    putStrLn "Melody loaded successfully!\n"
                    return $ Just (Melody (map fromJust elements))
                else do
                    putStrLn "Error: Invalid music element data in file! Please check the format."
                    return Nothing
        else do
            putStrLn "Error: File not found! Ensure the file path is correct."
            return Nothing
  where
    fromJust (Just x) = x
    fromJust Nothing  = error "Nothing cannot be converted to a value"

-- QuickCheck property for stringToNote
prop_stringToNote :: Note -> Bool
prop_stringToNote note =
    let noteStr = show (pitch note) ++ "," ++ durationToString (duration note)
    in stringToNote noteStr == Right note

-- Arbitrary instance for Duration to generate random durations for testing
newtype ArbitraryDuration = ArbitraryDuration Duration deriving (Show)

instance Arbitrary ArbitraryDuration where
    arbitrary = do
        n <- arbitrary
        d <- arbitrary `suchThat` (/= 0)
        return $ ArbitraryDuration (n % d)
