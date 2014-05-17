
isSeq :: (Enum a, Eq a) => [a] -> Bool
isSeq [] = True
isSeq (x:[]) = True
isSeq (x:y:zs) | y == succ x = isSeq $ y:zs
isSeq _ = False

tickHours :: [Int] -> [Int] -> Int -> [Int]
tickHours (ball:balls) hours 12 = balls ++ hours ++ [ball]
tickHours q hours count = (\(b:bs) -> tickHours bs (b:hours) (count + 1)) $ tickFives q [] 1

tickFives :: [Int] -> [Int] -> Int -> [Int]
tickFives q fives 12 = q ++ fives
tickFives q fives count = (\(b:bs) -> tickFives bs (b:fives) (count + 1)) $ tickOnes q [] 1

tickOnes :: [Int] -> [Int] -> Int -> [Int]
tickOnes q ones 5 = q ++ ones
tickOnes (ball:balls) ones count = tickOnes balls (ball:ones) (count + 1)
		
getDays :: Int -> [Int] -> [Int]
getDays 0 q = tickHours q [] 1
--getDays halfDays q = if isSeq q then quot halfDays 2
										 --else getDays 0 $ tickHours q [] 1

main = print $ getDays 0 [1..30]
