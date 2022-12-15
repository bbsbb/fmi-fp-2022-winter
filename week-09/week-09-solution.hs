import Data.Char
--
--


toDigits :: Int -> [Int]
toDigits n
  | n < 10 = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

isNarcissistic :: Int -> Bool
isNarcissistic n = n == sum [d ^ countDigits | d <- digits] where
  digits = toDigits n
  countDigits = length digits


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


-- 2018 / 05

polymersReact :: Char -> Char -> Bool
polymersReact c1 c2 = c1 /= c2 && (toLower c1) == (toLower c2)

foldPolymer :: [Char] -> [Char]
foldPolymer s = foldr (\ currentPolymer foldedPolymer -> if foldedPolymer /= "" && polymersReact (head foldedPolymer) currentPolymer
                                                         then tail foldedPolymer
                                                         else currentPolymer : foldedPolymer) [] s
