import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as Bui
import Data.Foldable 
import System.Process
import Text.Printf

ex_1 :: [Float]
ex_1 = [0.0..48000]

ex_2 :: [Float]
ex_2 = [0.0, 0.1..48000]

rate :: Float
rate = 48000.0

def_vol :: Float
def_vol =  0.5

def_step :: Float
def_step = 0.01

step :: Float -> Float
step x = 2 * x * pi / rate

middle_c :: Float
middle_c = 261.63

pitch_standard :: Float
pitch_standard = 440.0

pitch Float -> Float
pitch x = pitch_standard * (2 ** (x / 12))

duration :: Float
duration = 1.0

ex_3 :: [Float]
ex_3 = [0.0 .. duration * rate]

wave:: [Float] -> [Float]
wave [] = []
-- wave (x:xs) = map ( * def_vol) $ (map (\ y -> sin (y * def_step)) xs)
wave (x:xs) = map ( * def_vol) $ map sin $ map ( * step pitch_standard) (x:xs)

save:: FilePath -> [Float] -> IO ()
save xs ys = B.writeFile xs $ Bui.toLazyByteString $ fold $ map Bui.floatLE (wave ys)

-- almost organ like
sin_func_1:: Float -> Float
sin_func_1 x = sin (x) + sin (x/2) + cos (3 * x)

-- almost like a heart monitor
sin_func_2:: Float -> Float
sin_func_2 x = sin (x) + sin (5 * x) + sin (-3 * x)

-- almost like a 
sin_func_3:: Float -> Float
sin_func_3 x = sin (x) + cos (2 * x)

-- probably sounds like trash
sin_func_4:: Float -> Float
sin_func_4 x = sin (x) + sin (x / 2) + (3 * sin (x / 5))

-- probably sounds like an alarm
sin_func_5:: Float -> Float
sin_func_5 x = sin (x) + sin (5 * x) + 3 * (sin (x / 10))

-- probably sounds like an amber alert
sin_func_6:: Float -> Float
sin_func_6 x = sin (x) + sin (5 * x) + 3 * (sin (x / 10)) + 2 * (sin (x / 2))

-- probably sounds like a ??????
sin_func_7:: Float -> Float
sin_func_7 x = sin (x) + 2 * cos (sqrt (abs x) - 1.5)

play:: FilePath -> IO ()
play xs = do
    save xs ex_3
    _ <- runCommand $ printf "ffplay -f f32le -ar 48000 %s" xs
    return ()