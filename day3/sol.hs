import Data.List
main :: IO ()

p1 [] = 0
p1 (x:xs) = (read :: String -> Int) (helper x 2) + p1 xs

p2 [] = 0
p2 (x:xs) = (read :: String -> Int) (helper x 12) + p2 xs

-- 💀
helper l 0 = []
helper l c = maximum (reverse (drop (c-1) (reverse l))) : (helper (drop (1 + (\(Just x) -> x) (elemIndex (maximum (reverse (drop (c-1) (reverse l)))) l)) l) (c-1))
--                 largest number excluding the last (c-1)    //        call this function again with the sublist beginning after the largest number from this iteration
--                 because we still need (c-1) numbers         //

main = do
   input <- readFile "input.txt"
   let lines = words input

   print $ p1 lines
   print $ p2 lines