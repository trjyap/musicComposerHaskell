{-# LANGUAGE FlexibleInstances #-}
module MusicData where

import Test.QuickCheck (Arbitrary, arbitrary, suchThat)
import Data.Ratio ((%))

-- A music element can be a single note or a chord (multiple notes at the same time)
data MusicElement = SingleNote Note | ChordElement Chord
  deriving (Show, Eq)

-- A single note in a melody
type Pitch = Int        -- MIDI pitch value (e.g., 60 = Middle C)
type Duration = Rational   -- Duration as a fraction of a whole note (1 % 1 = whole, 1 % 4 = quarter, etc.)

data Note = Note
    { pitch :: Pitch,
    duration :: Duration
    } deriving (Show, Eq)

-- A list of notes played at the same time
newtype Chord = Chord [Note]
    deriving (Show, Eq)

-- A melody as a list of generic elements
newtype Melody a = Melody [a]
    deriving (Show, Eq)

-- Typeclass for playable music elements
class Playable a where
    play :: a -> IO ()

-- Instance for playing chords and notes
instance Playable MusicElement where
    play (SingleNote (Note p d)) = 
        putStrLn $ "Playing note: Pitch " ++ show p ++ ", Duration " ++ show d
    play (ChordElement (Chord notes)) = do
        putStrLn "Playing chord with these notes:"
        mapM_ printNote notes
        where
            printNote (Note p d) = putStrLn $ "  Pitch: " ++ show p ++ ", Duration: " ++ show d

-- Instance for playing melodies
instance Playable (Melody MusicElement) where
    play (Melody elements) = mapM_ play elements


-- Functor instance for Melody
instance Functor Melody where
    fmap f (Melody elements) = Melody (map f elements)

-- Applicative instance for Melody
instance Applicative Melody where
    pure element = Melody [element]
    (Melody fs) <*> (Melody xs) = Melody [f x | f <- fs, x <- xs]

-- Monad instance for Melody
instance Monad Melody where
    return = pure
    (Melody elements) >>= f = Melody $ concatMap (\x -> let (Melody xs) = f x in xs) elements

-- Semigroup instance for MusicElement
instance Semigroup (Melody MusicElement) where
    (Melody notes1) <> (Melody notes2) = Melody (notes1 <> notes2)

-- Monoid instance for MusicElement
instance Monoid (Melody MusicElement) where
    mempty = Melody []

-- Arbitrary instance for Note to generate random notes for testing
instance Arbitrary Note where
    arbitrary = do
        p <- arbitrary
        d <- arbitrary `suchThat` (/= 0)
        return $ Note p (d % 1)