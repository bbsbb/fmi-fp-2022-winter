import Data.Char

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys


mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeLists (mergeSort f) (mergeSort s) where
  (f, s) = splitAt (length xs `div` 2) xs


isPrime :: Int -> Bool
isPrime n = length [ x | x <- [2..(div n 2)], mod n x == 0] == 0

toPrime :: Int -> (Int, Int)
toPrime n = head [(x, n -x) | x <- [2..n-1], isPrime x && isPrime (n-x)]


encodeC :: Char -> Char -> Char
encodeC c1 c2 = chr $ ((ord (toLower c1) - 97) + ((ord (toLower c2)) - 97)) `mod` 26 + 97

encodeV :: String -> String -> String
encodeV [] _ = []
encodeV (c:text) (s:passphrase) = encodeC c s : encodeV text (passphrase ++ [s])

compose :: [(Int -> Int)] -> (Int -> Int)
compose fs = (\ i -> foldr (\ current result -> current result) i fs)

composeDot :: [(Int -> Int)] -> (Int -> Int)
composeDot fs = foldr1 (.) fs

--(compose [(\ a -> a * 10), (\ a -> a `div` 5)]) 4 => 5
