module MusicData (Melody, wordsWhen) where

-- data Note = Note { pitch :: String, duration :: Int }
-- type Melody = [Note]
-- data Chord = Chord [Note]
type Melody = [String] -- Placeholder for now

class Playable a where
    play :: a -> IO ()

-- Utility function to split a string by a delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

