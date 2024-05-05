import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as Bui
import Data.Foldable 
import System.Process
import Text.Printf
import Helper

-- rate :: Float
rate = 48000.0

-- def_step :: Float
def_step = 0.01

-- def_vol :: Float
def_vol =  0.5

def_bpm = 60.0

nC3 :: Float
nC3 = pitch (-9)

nCsharp3 :: Float
nCsharp3 = pitch (-8)

nD3 :: Float
nD3 = pitch (-7)

nDsharp3 :: Float
nDsharp3 = pitch (-6)

nE3 :: Float
nE3 = pitch (-5)

nF3 :: Float
nF3 = pitch (-4)

nFsharp3 :: Float
nFsharp3 = pitch (-3)

nG3 :: Float
nG3 = pitch (-2)

nGsharp3 :: Float
nGsharp3 = pitch (-1)

nA3 :: Float
nA3 = pitch_standard

nAsharp3 :: Float
nAsharp3 = pitch 1

nB3 :: Float
nB3 = pitch 2

nC4 :: Float
nC4 = pitch 3

nCsharp4 :: Float
nCsharp4 = pitch 4

nD4 :: Float
nD4 = pitch 5

nDsharp4 :: Float
nDsharp4 = pitch 6

nE4 :: Float
nE4 = pitch 7

nF4 :: Float
nF4 = pitch 8

nFsharp4 :: Float
nFsharp4 = pitch 9

nG4 :: Float
nG4 = pitch 10

nGsharp4 :: Float
nGsharp4 = pitch 11

step :: Float -> Float
step x = 2 * x * pi / rate

middle_c :: Float
middle_c = 261.63

-- A3
pitch_standard :: Float
pitch_standard = 440.0

pitch :: Float -> Float
pitch x = pitch_standard * (2 ** (x / 12))

def_duration :: Float
def_duration = 1.0

-- Note contains pitch, duration (in beats), and volume
data Note = Note Float Float Float
instance Show Note where
    show (Note p d v) = note_to_string (Note p d v)

-- Phrase contains a list of unprocessed notes
data Phrase = Phrase [Note]
instance Show Phrase where
    show (Phrase xs) = "Phrase: length " ++ printf "%d" (length xs)  ++ "\n" ++ (unlines $ map note_to_string xs)

-- Sound contains a list of processed notes in the form of a list of floats
data Sound = Sound [Float]
instance Show Sound where
    show (Sound xs) = "Sound: length " ++ show (length xs)

note_to_string :: Note -> String
note_to_string (Note p d v) = printf "Note: pitch %f, duration %f, volume %f" p d v


make_note :: Float -> Float -> Float -> Note
make_note p d v = Note p d v

make_phrase :: [Note] -> Phrase
make_phrase xs = Phrase xs

process_note :: Note -> Sound
process_note (Note p d v) = make_wave p d def_bpm v

concat_sound :: Sound -> Sound -> Sound
concat_sound (Sound x) (Sound y) = Sound (x ++ y)

process_phrase :: Phrase -> Sound
process_phrase (Phrase []) = Sound []
process_phrase (Phrase (x:xs)) = concat_sound (process_note x) (process_phrase (Phrase xs))

change_note_speed :: Note -> Float -> Note
change_note_speed (Note p d v) x = Note p (d * x) v

change_phrase_speed :: Phrase -> Float -> Phrase
change_phrase_speed (Phrase []) x = Phrase []
change_phrase_speed (Phrase (y:ys)) x = make_phrase $ map (\ z -> change_note_speed z x) (y:ys)

change_note_volume :: Note -> Float -> Note
change_note_volume (Note p d v) x = Note p d (v * x)

change_phrase_volume :: Phrase -> Float -> Phrase
change_phrase_volume (Phrase []) x = Phrase []
change_phrase_volume (Phrase (y:ys)) x = make_phrase $ map (\ z -> change_note_volume z x) (y:ys)

-- change_note_pitch :: Note -> Float -> Note

-- change_phrase_key :: Phrase -> Float -> Phrase

add_note :: Phrase -> Note -> Phrase
add_note (Phrase xs) y = make_phrase (xs ++ [y])

add_phrase :: Phrase -> Phrase -> Phrase
add_phrase (Phrase xs) (Phrase ys) = make_phrase (xs ++ ys)

remove_note_index :: Phrase -> Int -> Phrase
remove_note_index (Phrase []) _ = make_phrase []
remove_note_index (Phrase xs) 0 = make_phrase (tail xs)
remove_note_index (Phrase (x:xs)) y = add_phrase (make_phrase (x:[]))  (remove_note_index (make_phrase xs) (y - 1))

-- pitch_equal :: Note -> (either Note Float) -> Bool
-- -- pitch_equal (Note x _ _) (Note y _ _) = x == y
-- pitch_equal (Note x _ _) (Left (Note y _ _)) = x == y
-- pitch_equal (Note x _ _) (Right y) = x == y

-- note_equal :: Note -> Note -> Bool
-- note_equal (Note x y z) (Note a b c) = x == a && y == b && z == c

-- remove_match_pitch :: Phrase -> Float -> Phrase
-- remove_match_pitch (Phrase []) _ = make_phrase []
-- remove_match_pitch (Phrase (x:xs)) y = 

def_wave:: [Float]
def_wave = map ( * def_vol) $ map sin $ map ( * step pitch_standard) [0.0 .. def_duration * rate * 60 / def_bpm]

make_wave:: Float -> Float -> Float -> Float -> Sound
make_wave pitch duration bpm vol = Sound (map ( * vol) $ map sin $ map ( * step pitch) [0.0 .. duration * rate * 60 / bpm])
-- wave (x:xs) = map ( * def_vol) $ (map (\ y -> sin (y * def_step)) xs)

def_save:: FilePath -> IO ()
def_save xs = B.writeFile ("bin/" ++ xs) $ Bui.toLazyByteString $ fold $ map Bui.floatLE (def_wave)

save:: FilePath -> Sound -> IO ()
save xs (Sound ys) = B.writeFile ("bin/" ++ xs) $ Bui.toLazyByteString $ fold $ map Bui.floatLE ys

play:: FilePath -> IO ()
play xs = do
    _ <- runCommand $ printf "ffplay -f f32le -ar 48000 bin/%s" xs
    return ()

play_sound:: FilePath -> Sound -> IO ()
play_sound xs (Sound ys) = do
    save xs (Sound ys)
    _ <- runCommand $ printf "ffplay -f f32le -ar 48000 bin/%s" xs
    return ()

play_phrase:: FilePath -> Phrase -> IO ()
play_phrase xs (Phrase ys) = do
    save xs (process_phrase (Phrase ys))
    _ <- runCommand $ printf "ffplay -f f32le -ar 48000 bin/%s" xs
    return ()





