
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | shifted > 0 = (mod n 10) : (toDigitsRev shifted)
    | otherwise = [n]
    where shifted = (div n 10)

--takes a number and a count and returns the highest digit and which digit it is 10^x
highestDigit :: Integer -> Integer
highestDigit x
    | (div x 10) > 0 = highestDigit (div x 10)
    | otherwise = x

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | shifted > 0 = flip (++) [(mod n 10)] (toDigits shifted)
    | otherwise = [n]
    where shifted = (div n 10)

listLength :: [Integer] -> Integer
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:xs))
    | (mod (listLength xs) 2) < 1 = (2*x):y:(doubleEveryOther xs)
    | otherwise = x:(2*y):(doubleEveryOther xs)

addedDigits :: Integer -> Integer
addedDigits n
    | n < 10 = n
    | otherwise = (mod n 10) + (addedDigits (div n 10))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (addedDigits x)+(sumDigits xs)

validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0

main = do
    putStrLn $ show (validate  4012888888881881)
    putStrLn $ show (validate  4012888888881882)
    putStrLn $ show (toDigits 12)
