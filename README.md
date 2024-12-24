# Musical Composer with Functional Patterns

This is a project for course PRG2214 Functional Programming Principles (Sunway University). It is system that lets the user create melodies for playback by specifying the pitch and duration. Other functions including saving, loading, transposing, and combining melodies. The playback of melodies is presented in text form showing the pitch and duration of each note/chord instance. 

The system utilises Haskell as a means of practicing implementing functional programming techniques. Instances such as functors and semigroups are used to allow melody shaping methods to be performed at an abstract level. 

There was a plan to use third-party libraries like Euterpea and hmidi so that output could be in midi and also playable/audible. However, there were compatibility issues with ghc, cabal, and even the OS. Given the time constraints, these efforts were scrapped in favour of releasing a working project without midi implementation. Hence the pitch values are still in Int to correlate with midi values, where it could possibly be worked on in the future. 
