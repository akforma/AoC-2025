import Data.List.Split

main :: IO ()

p1 [] = 0
p1 (x:xs) = checkId1 (map show x) + p1 xs

p2 [] = 0
p2 (x:xs) = checkId2 (map show x) + p2 xs

-- accumulate if first half of ID is the same as second half
checkId1 :: [String] -> Int
checkId1 [] = 0
checkId1 (x:xs)
   | take (length x `div` 2) x == drop (length x `div` 2) x = (read :: String -> Int) x + checkId1 xs
   | otherwise = checkId1 xs

-- accumulate if the ID is made up of chunks of the same sequence of numbers
checkId2 :: [String] -> Int
checkId2 [] = 0
checkId2 (x:xs) = helper 1 x + checkId2 xs
   where
      helper n id
         | n > (length id `div` 2) = 0
         | checkSame (chunksOf n id) = (read :: String -> Int) id
         | otherwise = helper (n+1) id

checkSame :: [String] -> Bool
checkSame (x:xs:[]) = x == xs
checkSame (x:xs:xss) = x == xs && checkSame (xs:xss)
checkSame _ = True

main = do
   input <- readFile "input.txt"
   let ranges = [[(read :: String -> Int) a .. (read :: String -> Int) b] | [a,b] <- map (splitOn "-") $ splitOn "," input]

   print $ p1 ranges
   print $ p2 ranges