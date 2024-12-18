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

-- Creates a note
createNote :: Pitch -> Duration -> Note
createNote p d = Note { pitch = p, duration = d }

-- Prompts the user to create a new melody
createMelody :: IO Melody
createMelody = do
    putStrLn "Enter notes for your melody (space-separated, e.g., 60,1%4 62,1%4): "
    input <- getLine
    let noteStrings = words input -- Space-separated notes
    let notes = map stringToNote noteStrings
    case sequence notes of
        Left err -> do
            putStrLn $ "Error: " ++ err
            createMelody -- Retry on error
        Right validNotes -> do
            putStrLn "Melody created successfully!"
            return $ Melody validNotes


-- Converts a single Note to a String format "pitch,duration"
noteToString :: Note -> String
noteToString (Note p d) = show p ++ "," ++ show d

-- Saves the melody to a specified file
saveMelody :: FilePath -> Melody -> IO ()
saveMelody filePath (Melody notes) = do
    let content = unlines $ map noteToString notes
    writeFile filePath content
    putStrLn $ "Melody saved to " ++ filePath

-- Parses a single line into a Note
parseNote :: String -> Maybe Note
parseNote line = 
    case stringToNote line of
        Right note -> Just note
        Left _ -> Nothing 

-- Loads a melody from a specified file
loadMelody :: FilePath -> IO (Maybe Melody)
loadMelody filePath = do
    exists <- doesFileExist filePath
    if exists
        then do
            content <- readFile filePath
            let notes = map parseNote (lines content)
            if all isJust notes
                then do
                    putStrLn "Melody loaded successfully!"
                    return $ Just (Melody (map fromJust notes))
                else do
                    putStrLn "Error: Invalid note data in file!"
                    return Nothing
        else do
            putStrLn "Error: File not found!"
            return Nothing
  where
    isJust (Just _) = True
    isJust _        = False
    fromJust (Just x) = x
    fromJust Nothing  = error "Nothing cannot be converted to a value"
