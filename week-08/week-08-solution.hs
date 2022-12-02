import Data.List

-- What is Haskell?

-- *is* Statically Typed
-- *is* Strongly Typed
-- *is* lazy evaluated
-- *is* currying functions by default.
-- What are interfaces?
-- *has* Typeclasses
-- Has multiple types of polymorphism available
-- G: Google the expression problem
-- *has* List comprehension
-- *has* Pattern matching
-- Will see ADTs

iSum :: Int -> Int -> Int
iSum m n = m + n

allInts :: Integer -> [Integer]
allInts n = n : allInts (n + 1)

countDigits :: Integer -> Integer
countDigits n
  | n < 10 = 1
  | otherwise = 1 + (countDigits $ n `div` 10)


iPoop :: Integer -> Integer
iPoop 5 = 50
iPoop 7 = 7

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

elfFoodHelper :: [Integer] -> Integer -> Integer
elfFoodHelper [] currentElfFood = currentElfFood
elfFoodHelper (0:food) currentElfFood = max currentElfFood $ elfFoodHelper food 0
elfFoodHelper (snack:food) currentElfFood = elfFoodHelper food (currentElfFood + snack)

elfFood :: [Integer] -> Integer
elfFood food = elfFoodHelper food 0

--elfFoodTop3 :: [Integer] -> [Integer]


allElfsWithCarry :: [Integer] -> Integer -> [Integer]
allElfsWithCarry [] currentElf = [currentElf]
allElfsWithCarry (0:food) currentElf = [currentElf] ++ (allElfsWithCarry food 0)
allElfsWithCarry (snack:food) currentElf = allElfsWithCarry food (currentElf + snack)

allElfs :: [Integer] -> [Integer]
allElfs food = take 3 $ reverse $ sort (allElfsWithCarry food 0)
