

import Data.Char

data Melody = Chord [Note] | Sequence [Melody] | Rest | Note
    deriving (Show,Eq)

data Chord = Chord [Note]
    deriving (Show,Eq)

data Note = Note Pitch Duration 

track0 = [(0,  NoteOn 0 60 80),
          (24, NoteOff 0 60 0),
          (0,  TrackEnd)]