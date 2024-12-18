module MusicData where

-- A single note in a melody
type Pitch = Int        -- MIDI pitch value (e.g., 60 = Middle C)
type Duration = Rational   -- Duration as a fraction of a whole note (1 % 1 = whole, 1 % 4 = quarter, etc.)

data Note = Note 
    { pitch :: Pitch, 
    duration :: Duration 
    } deriving (Show, Eq)

-- A list of notes representing a melody
newtype Melody = Melody [Note] 
    deriving (Show, Eq)

-- A list of notes played at the same time
newtype Chord = Chord [Note] 
    deriving (Show, Eq)

-- Typeclass for playable music elements
class Playable a where
    play :: a -> IO ()

-- Instance for Melody
instance Playable Melody where
    play (Melody notes) = mapM_ printNote notes
        where
            printNote (Note p d) = putStrLn $ "Playing note: Pitch " ++ show p ++ ", Duration " ++ show d

-- Instance for Chord
instance Playable Chord where
    play (Chord notes) = do
        putStrLn "Playing chord:"
        mapM_ printNote notes
        where
            printNote (Note p d) = putStrLn $ "  Pitch: " ++ show p ++ ", Duration: " ++ show d


-- Function to transpose a note by a given number of semitones
transposeNote :: Int -> Note -> Note
transposeNote interval note = note { pitch = pitch note + interval }

-- Function to transpose an entire melody
transposeMelody :: Int -> [Note] -> [Note]
transposeMelody interval = map (transposeNote interval)

