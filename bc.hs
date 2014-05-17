
isSeq :: (Enum a, Eq a) => [a] -> Bool
isSeq [] = True
isSeq (x:[]) = True
isSeq (x:y:zs) | y == succ x = isSeq $ y:zs
isSeq _ = False

tick :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> Integer -> Integer
tick q@(ball:balls) ones fives hours m
	| isSeq q && m > 5 = quot m 1440
	| m `mod` 720 == 0 = tick (balls ++ ones ++ fives ++ hours ++ [ball]) [] [] [] (m + 1) 
	| m `mod` 60 == 0 = tick (balls ++ ones ++ fives) [] [] (ball:hours) (m + 1)
	|	m `mod` 5 == 0 = tick (balls ++ ones) [] (ball:fives) hours (m + 1)
	| otherwise = tick balls (ball:ones) fives hours (m + 1)

getDays :: Integer -> Integer
getDays n = let q = [1..n] in
						tick q [] [] [] 1

main = do
	print $ getDays 123
