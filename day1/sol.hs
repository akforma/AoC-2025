main :: IO ()

p1Add 0 = 1
p1Add _ = 0

p2Add 0 'L' ls = ls `div` 100
p2Add num 'L' ls = (100 - num + ls) `div` 100
p2Add num 'R' rs = (num + rs) `div` 100

p1 [] _ = 0
p1 (('L':ls):xs) num = p1Add newNum + p1 xs newNum where newNum = (num - read ls) `mod` 100
p1 (('R':rs):xs) num = p1Add newNum + p1 xs newNum where newNum = (num + read rs) `mod` 100

p2 [] _ = 0
p2 (('L':ls):xs) num = p2Add num 'L' (read ls) + p2 xs ((num - read ls) `mod` 100)
p2 (('R':rs):xs) num = p2Add num 'R' (read rs) + p2 xs ((num + read rs) `mod` 100) 

main = do
   input <- readFile "input.txt"
   let wds = words input

   print $ p1 wds 50
   print $ p2 wds 50