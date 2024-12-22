module MIDIWriter where

-- import Sound.MIDI.File
-- import Sound.MIDI.File.Save (toFile)
-- import Sound.MIDI.Message.Channel.Voice (T(NoteOn), T(NoteOff))
-- import System.Info (os)
-- import System.Process (callCommand)
-- import MusicData

-- -- Converts a single Note to MIDI events
-- noteToMidiEvent :: Note -> [Track Ticks]
-- noteToMidiEvent (Note p d) =
--     let durationTicks = round (d * 480)  -- Assuming 480 ticks per beat
--     in [ [(0, VoiceEvent (NoteOn 0 p 64))]              -- NoteOn event (channel 0, velocity 64)
--        , [(durationTicks, VoiceEvent (NoteOff 0 p 64))] -- NoteOff event after the duration
--        ]

-- -- Converts a Melody to a MIDI track
-- melodyToMidi :: Melody -> [Track Ticks]
-- melodyToMidi (Melody notes) = concatMap noteToMidiEvent notes

-- -- Saves a melody as a MIDI file
-- saveMelodyAsMidi :: FilePath -> Melody -> IO ()
-- saveMelodyAsMidi filePath melody = do
--     let track = melodyToMidi melody
--     let midi = File Type0 (TicksPerBeat 480) [track]
--     toFile filePath midi
--     putStrLn $ "MIDI file saved to: " ++ filePath

-- -- Plays a MIDI file using the OS default MIDI player
-- playMidiFile :: FilePath -> IO ()
-- playMidiFile filePath = do
--     putStrLn $ "Playing MIDI file: " ++ filePath
--     case os of
--         "darwin"  -> callCommand $ "open " ++ filePath       -- macOS
--         "linux"   -> callCommand $ "xdg-open " ++ filePath   -- Linux
--         "windows" -> callCommand $ "start " ++ filePath      -- Windows
--         _         -> putStrLn "Unsupported operating system for MIDI playback"
