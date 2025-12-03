import Data.List
main :: IO ()

p1 [] = 0
p1 (x:xs) = (read :: String -> Int) (helper x 2) + p1 xs

p2 [] = 0
p2 (x:xs) = (read :: String -> Int) (helper x 12) + p2 xs

-- 💀
helper x 0 = []
helper x c = maximum (reverse (drop (c-1) (reverse x))) : (helper (drop (1 + (\(Just x) -> x) (elemIndex (maximum (reverse (drop (c-1) (reverse x)))) x)) x) (c-1))

main = do
   input <- readFile "input.txt"
   let lines = words input

   print $ p1 lines
   print $ p2 lines