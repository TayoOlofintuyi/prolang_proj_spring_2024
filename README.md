# SinWaves

For Programming Languages (CS 3490) Spring 2024

By: Tayo Olofintuyi

## Description

This project is a Haskell library that generates sin waves from musical notes and phrases. It provides functions to create and manipulate notes and phrases, convert them to sound waves, and play or save the sound waves to a file.

# Getting Started

## Installation

Clone this repository to your local machine or download the zip file and extract it.

Cloning with HTTPS:
    ```$ git clone https://github.com/TayoOlofintuyi/prolang_proj_spring_2024.git```

Cloning with SSH:
    ```$ git clone git@github.com:TayoOlofintuyi/prolang_proj_spring_2024.git ```

Open a terminal and navigate to the project root directory. If you do not already have it installed, install Haskell. Instructions for installing Haskell can be found [here](https://www.haskell.org/get-started/).

Also, if you have not already, install FFmpeg. Instructions for installing FFmpeg on Windows can be found [here](https://www.gyan.dev/ffmpeg/builds/). Instructions for installing FFmpeg on MacOS can be found [here](https://phoenixnap.com/kb/ffmpeg-mac). Instructions for installing FFmpeg on Linux can be found [here](https://phoenixnap.com/kb/install-ffmpeg-ubuntu).

Once you have Haskell and FFmpeg installed, start the GHCI session by running
    ```$ ghci```

Then in the GHCI session, load the necessary modules by running
    ```$ :l driver.hs```

Though `driver.hs` is not the primary file of the project (and it does not act as a driver), it contains the necessary imports and functions to run the project. By loading `driver.hs`, you will also load `sinewaves.hs`, which *is* the main file of the project and contains the necessary functions and data types which can be read about in the API Reference section. What `driver.hs` actually contains is a few examples `Phrases` that can be manipulated and a `sinewaves.hs` import statement.

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

### `pitch` :: Float -> Float

Takes in a number and converts it to a pitch that is that many half steps/semitones above the `pitch_standard` of A4.

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

### `play` :: FilePath -> IO ()

Plays a sound from the file path provided.

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

### `change_note_volume` :: Note -> Float -> Note

Changes the volume of a note. The float provided scales the originally volume of the note.

### `change_phrase_volume` :: Phrase -> Float -> Phrase

Changes the volume of a phrase by changing the volume of each Note. The float provided scales the original volume of each note the phrase.

### `set_note_pitch` :: Note -> Float -> Note

Changes the pitch of a note. The float provided replaces the originally pitch of the note.

### `scale_note_pitch` :: Note -> Float -> Note

Changes the pitch of a note. The float provided scales the original pitch of the note.

### `change_phrase_key` :: Phrase -> Float -> Phrase

Changes the key of a phrase by changing the pitch of each Note. The float provided scales the original pitch of each note in the phrase.

### `add_note` :: Phrase -> Note -> Phrase

Adds a note to a phrase.

### `add_phrase` :: Phrase -> Phrase -> Phrase

Adds a phrase to another phrase.

### `remove_note_index` :: Phrase -> Int -> Phrase

Removes a note from a phrase at the given index.

### `pitch_equal` :: Note -> Note -> Bool

Checks if two notes have the same pitch.

### `note_equal` :: Note -> Note -> Bool

Checks if two notes are equal.

### `remove_match_pitch` :: Phrase -> Float -> Phrase

Removes all notes with the given pitch from a phrase.

### `remove_match_note` :: Phrase -> Note -> Phrase

Removes all notes equal to the given note from a phrase.

### `insert_note` :: Phrase -> Note -> Int -> Phrase

Inserts a note into a phrase at the given index.

### `insert_phrase` :: Phrase -> Phrase -> Int -> Phrase

Inserts a phrase into another phrase at the given index. The first parameter is the phrase to insert into, the second parameter is the phrase to insert, and the third parameter is the index to insert the phrase at.

### `replace_note_index` :: Phrase -> Note -> Int -> Phrase

Replaces a note in a phrase at the given index.

### `find_replace_note` :: Phrase -> Note -> Note -> Phrase

Finds and replaces all notes equal to the first note with the second note in a phrase.

### `get_note_index` :: Phrase -> Int -> [Maybe Note]

Gets the note at the given index in a phrase.

### `make_wave` :: Float -> Float -> Float -> Float -> Sound

Creates a Sound with the given pitch, duration, bpm, and volume respectively.





