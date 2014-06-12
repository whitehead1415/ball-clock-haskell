data Hold = Hold { q :: [Int], row :: [Int] } deriving (Show)

isReset :: [Int] -> Int -> Bool
isReset []     _ = True
isReset (x:xs) i
  | x /= i = False
  | otherwise = isReset xs (i + 1)

split ::[Int] -> Hold -> Hold
split r (Hold {q = q, row = b:bs}) = Hold (q ++ bs) (b:r)

tick :: Int -> [Int] -> [Int] -> Hold
tick 0 q      newList   = Hold q newList
tick i (x:xs) newList   = tick (i-1) xs (x : newList)

tickFive :: Int -> Hold -> Hold
tickFive 0 hold = hold
tickFive i (Hold {q = q, row = r}) = tickFive (i - 1) $ split r $ tick 5 q []

tickHour :: Int -> Hold -> Hold
tickHour 0 hold = hold
tickHour i (Hold {q = q, row = r}) = tickHour (i - 1) $ split r $ tickFive 12 $ Hold q []

tickHalfDay :: [Int] -> [Int]
tickHalfDay q = (\ (Hold {q = q, row = b:bs}) -> q ++ bs ++ [b]) $ tickHour 12 $ Hold q []

go :: Int -> [Int] -> Int
go i q
  | isReset q 1 && i /= 0 = i
  | otherwise = go (i + 1) (tickHalfDay (tickHalfDay q))

main = print $ go 0 [1..30]

