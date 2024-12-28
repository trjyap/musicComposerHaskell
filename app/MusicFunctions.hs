{-# LANGUAGE FlexibleInstances #-}
module MusicFunctions where

import MusicData
import System.Directory (doesFileExist)
import Data.Ratio (numerator, denominator)
import Test.QuickCheck (quickCheck)

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
            _                     -> Left "Invalid pitch or duration format."
    _ -> Left "Invalid note format. Use 'pitch,duration' (e.g., '60,1%4').\n"

-- Converts a string to a MusicElement
stringToMusicElement :: String -> Either String MusicElement
stringToMusicElement str =
    case wordsWhen (== '|') str of
        [elementStr] -> parseElement elementStr
        _            -> Left "Invalid format. Use '|' to separate elements (notes or chords).\n"
  where
    parseElement :: String -> Either String MusicElement
    parseElement elementStr =
        -- Try parsing as a single note
        case stringToNote elementStr of
            Right note -> Right (SingleNote note)
            Left _     ->
                -- Try parsing as a chord: multiple notes separated by spaces
                let noteStrings = words elementStr
                in if length noteStrings > 1
                    then
                        let notes = map stringToNote noteStrings
                        in case sequence notes of
                            Left err -> Left $ "Invalid chord format. " ++ err
                            Right validNotes -> Right (ChordElement (Chord validNotes))
                    else Left "Invalid input format. Please check for commas and spaces.\n"

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
    putStrLn "Enter notes and chords for your melody (separate with \"|\"). Example: 60,1%4 64,1%4 67,1%4|62,1%4 65,1%4 69,1%4|75,1%1"
    putStrLn "Use the format 'pitch,duration where pitch is an integer and duration is a fraction (e.g., 60,1%4)."
    putStrLn "For chords, separate notes with a space (e.g., 60,1%4 64,1%4 67,1%4): "
    input <- getLine
    let elementStrings = wordsWhen (== '|') input -- Pipe-separated notes and chords
    let elements = map stringToMusicElement elementStrings
    case sequence elements of
        Left err -> do
            putStrLn $ "Error: " ++ err
            createMelody -- Retry on error
        Right validElements -> do
            putStrLn "Melody created successfully!\n"
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

-- Loads a melody from a specified file
loadMelody :: FilePath -> IO (Maybe (Melody MusicElement))
loadMelody filePath = do
    exists <- doesFileExist filePath
    if exists
        then do
            content <- readFile filePath
            let elements = map stringToMusicElement (lines content)
            if all isRight elements
                then do
                    putStrLn "Melody loaded successfully!\n"
                    return $ Just (Melody (map fromRight' elements))
                else do
                    putStrLn "Error: Invalid music element data in file! Please check the format.\n"
                    return Nothing
        else do
            putStrLn "Error: File not found! Ensure the file path is correct.\n"
            return Nothing
  where
    isRight (Right _) = True
    isRight _         = False
    fromRight' (Right x) = x
    fromRight' (Left _)  = error "Unexpected Left value"


-- ====================== Extra functions =========================
-- Function to combine two melodies into one
combineMelodies :: Melody MusicElement -> Melody MusicElement -> Melody MusicElement
combineMelodies melody1 melody2 = melody1 <> melody2

-- Function to transpose a note by a given number of semitones
transposeNote :: Int -> Note -> Note
transposeNote interval note = note { pitch = pitch note + interval }

-- Function to transpose an entire melody
transposeMelody :: Int -> Melody MusicElement -> Melody MusicElement
transposeMelody interval = fmap transposeElement
  where
    transposeElement (SingleNote note) = SingleNote (transposeNote interval note)
    transposeElement (ChordElement (Chord notes)) =
        ChordElement (Chord (map (transposeNote interval) notes))


-- ===================== QuickCheck functions =====================
-- QuickCheck property for transposeNote
prop_transposeNote :: Int -> Note -> Bool
prop_transposeNote interval note =
    let transposedNote = transposeNote interval note
    in pitch transposedNote == pitch note + interval

-- QuickCheck property for stringToNote
prop_stringToNote :: Note -> Bool
prop_stringToNote note =
    let noteStr = show (pitch note) ++ "," ++ durationToString (duration note)
    in stringToNote noteStr == Right note

runQuickCheckTests :: IO ()
runQuickCheckTests = 
    putStrLn "Running QuickCheck tests..." >>
    quickCheck prop_transposeNote >>
    quickCheck prop_stringToNote