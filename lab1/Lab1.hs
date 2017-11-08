{-
Lab 1 - Vaios Taxiarchis
-}
import Test.QuickCheck

-- Power function given in lecture
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1
power_steps :: Integer -> Integer -> Integer
power_steps n k | k < 0 = error "power: negative argument"
                | k == 0 = 1
                | otherwise = 1 + power_steps n (k-1)

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product l
  where l = replicate (fromInteger k) (fromInteger n)

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k  | k < 0 = error "power: negative argument"
            | k == 0 = 1
            | odd k = n * power2 n (k-1)
            | otherwise = power2 (n * n) (k `div` 2)

-- Part 4.A
{-
  -Test cases-
  1. power (2, 10) - base case, should return 1024
  2. power (2, 0) - power of n to 0 returns always 1
  3. power (-2, 10) - negative base and even power, returns positive number
  4. power (-2, 9) - negative base and odd power, returns negative number
  5. power (2, -1) - power of n to k<0, should raise the exception

  -Inputs allowed-
  1. Functions allow only integer values for n and k
  2. Functions catch negative integer values which are not allowed
-}

-- Part 4.B
prop_power n k = let k' = abs k in
  power n k' == power1 n k'
    && power n k' == power2 n k'
      && power1 n k' == power2 n k'

-- Part 4.C
-- List of tuples with test cases (4.A)
testCases :: [(Integer, Integer)]
testCases = [(2, 10), (2, 0), (-2, 10), (-2, 9), (2, -1)]

-- Function to get the first element from tupple
getFirst :: (a, b) -> a
getFirst (a, b) = a

-- Function to get the second element from tupple
getSecond :: (a, b) -> b
getSecond (a, b) = b

-- Function to check the test cases (4.A)
-- takes as input a tupple of integers and returns a boolean value
checkTestCases :: [(Integer, Integer)] -> Bool
checkTestCases [] = True
checkTestCases (a:ab) = (prop_power (getFirst a) (getSecond a)) && checkTestCases ab

-- Part 4.D
-- +++ OK, passed 100 tests.
