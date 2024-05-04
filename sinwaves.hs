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

play:: FilePath -> IO ()
play xs = do
    save xs ex_3
    _ <- runCommand $ printf "ffplay -f f32le -ar 48000 %s" xs
    return ()