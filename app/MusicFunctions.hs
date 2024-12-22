module MusicFunctions where

import MusicData
import System.Directory (doesFileExist)

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
    _ -> Left "Invalid note format. Use 'pitch,duration' (e.g., '60,1%4')."

-- Converts a string to a MusicElement
stringToMusicElement :: String -> Either String MusicElement
stringToMusicElement str = 
    case wordsWhen (== ',') str of
        [pitchStr, durStr] -> 
            case (reads pitchStr, reads durStr) of
                ([(p, "")], [(d, "")]) -> Right (SingleNote (Note p d))
                _ -> Left "Invalid pitch or duration format for note."
        _ -> case mapM stringToNote (words str) of
                Right notes -> Right (ChordElement (Chord notes))
                Left _ -> Left "Invalid chord format. Chords should have multiple notes."

-- Creates a note
createNote :: Pitch -> Duration -> Note
createNote p d = Note { pitch = p, duration = d }

-- Prompts the user to create a new melody
createMelody :: IO (Melody MusicElement)
createMelody = do
    putStrLn "Enter notes and chords for your melody (space-separated). For chords, separate notes with a comma (e.g., 60,1%4 62,1%4 or 60,1%4 62,1%4,64,1%4): "
    input <- getLine
    let elementStrings = words input -- Space-separated notes
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
noteToString (SingleNote (Note p d)) = show p ++ "," ++ show d
noteToString (ChordElement (Chord notes)) = unwords (map (noteToString . SingleNote) notes)

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
                    putStrLn "Melody loaded successfully!"
                    return $ Just (Melody (map fromJust elements))
                else do
                    putStrLn "Error: Invalid music element data in file!"
                    return Nothing
        else do
            putStrLn "Error: File not found!"
            return Nothing
  where
    isJust (Just _) = True
    isJust _        = False
    fromJust (Just x) = x
    fromJust Nothing  = error "Nothing cannot be converted to a value"
