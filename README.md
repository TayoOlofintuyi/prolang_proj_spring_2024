# SinWaves

For Programming Languages (CS 3490) Spring 2024

By: Tayo Olofintuyi

## Description

This project is a Haskell library that generates sin waves from musical notes and phrases. It provides functions to create and manipulate notes and phrases, convert them to sound waves, and play or save the sound waves to a file.

# API Reference

## Data Types

### `Note` 

A representation of a musical note. Contains pitch (Float), duration (Float), and volume (Float).

### `Phrase`

A representation of a musical phrase. Contains a list of `Note`s ([`Note`]).

### `Sound`

A representation of a sound as a list of values within a sine wave [`Float`]. 

## Constants

### `pitch_standard` :: Float

A constant representing the standard pitch of A4 (440 Hz).

### `def_duration` :: Float

A constant representing the default duration of a note (1 second).

### `def_vol` :: Float

A constant representing the default volume of a note (1).

### `middle_c` :: Float

A constant representing the pitch of middle C (261.63 Hz).

### -2 Octaves of Note Names-
    nC3 :: Float
    nCsharp3 :: Float
    nD3 :: Float
    nDsharp3 :: Float
    nE3 :: Float
    nF3 :: Float
    nFsharp3` :: Float
    nG3 :: Float
    nGsharp3 :: Float
    nA3 :: Float
    nAsharp3 :: Float
    nB3 :: Float
    nC4 :: Float
    nCsharp4 :: Float
    nD4 :: Float
    nDsharp4 :: Float
    nE4 :: Float
    nF4 :: Float
    nFsharp4 :: Float
    nG4 :: Float
    nGsharp4 :: Float
    nA4 :: Float

Representing the pitches of the notes in 2 octaves.

## Functions

### `make_note` :: Float -> Float -> Float -> Note

Creates a note with the given pitch, duration, and volume.

### `make_phrase` :: [Note] -> Phrase

Creates a phrase with the given list of notes.

### `process_note` :: Note -> Sound

Creates a sound from a note.

### `process_phrase` :: Phrase -> Sound

Creates a sound from a phrase.

### `note_to_string` :: Note -> String

Converts a note to a string.

### `phrase_to_string` :: Phrase -> String

Converts a phrase to a string.

### `concat_sound` :: Sound -> Sound -> Sound

Concatenates two sounds.

### `process_phrase` :: Phrase -> Sound

Creates a sound from a phrase.

### `play_sound` :: FilePath -> Sound -> IO ()

Plays a sound. requires a file path to write the sound to.

### `play_phrase` :: FilePath -> Phrase -> IO ()

Plays a phrase. requires a file path to write the sound to.

### `save` :: FilePath -> Sound -> IO ()

Saves a sound to a file. Requires a file path to write the sound to.

### `change_note_speed` :: Note -> Float -> Note

Changes the duration of a Note. The float provided is directly proportional to the duration of the note.

### `change_phrase_speed` :: Note -> Float -> Note

Changes the duration of a phrase by changing the duration if each Note. The float provided is directly proportional to the duration of the phrase.

### `set_note_pitch` :: Note -> Float -> Note

Changes the pitch of a note. The float provided replaces the originally pitch of the note.

### 




