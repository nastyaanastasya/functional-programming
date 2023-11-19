module MyLib (someFunc, zipLong) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

zipLong :: [a] -> [b] -> [(a,b)]
zipLong [] _ = []
zipLong _ [] = []
zipLong xs ys = take (max (length xs) (length ys)) $ zipWith (\x y -> (x, y)) (cycle xs) ys