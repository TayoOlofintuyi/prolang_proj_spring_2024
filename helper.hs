module Helper where

-- sin funcs!
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

-- example floats

ex_1 :: [Float]
ex_1 = [0.0..48000]

ex_2 :: [Float]
ex_2 = [0.0, 0.1..48000]

ex_3 :: [Float]
ex_3 = [0.0 .. 1.0 * 48000]


