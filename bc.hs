
isSeq :: (Enum a, Eq a) => [a] -> Bool
isSeq [] = True
isSeq (x:[]) = True
isSeq (x:y:zs) | y == succ x = isSeq $ y:zs
isSeq _ = False

tick :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> Integer -> Integer
tick q@(ball:balls) ones fives hours m =
	if m `mod` 5 == 0
		then if m `mod` 60 == 0
			then if m `mod` 720 == 0 
				then
					if isSeq (balls ++ ones ++ fives ++ hours ++ [ball]) 
						then quot m 1440
					else tick (balls ++ ones ++ fives ++ hours ++ [ball]) [] [] [] (m + 1) 
			else tick (balls ++ ones ++ fives) [] [] (ball:hours) (m + 1)
		else tick (balls ++ ones) [] (ball:fives) hours (m + 1)
	else tick balls (ball:ones) fives hours (m + 1)
		

getDays :: Integer -> Integer
getDays n = tick [1..n] [] [] [] 1

main = do
	print $ getDays 123
