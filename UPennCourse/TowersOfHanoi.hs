type Peg = String
type Move =(Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 0 = []
    | otherwise = (hanoi (n - 1) a c b) ++ [(a, c)] ++ (hanoi (n-1) b a c)


main = do
    putStrLn $ show (hanoi 3 "a" "b" "c")

