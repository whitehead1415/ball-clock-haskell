import Data.Sequence

data Hold = Hold { q :: Seq Int, row :: Seq Int} deriving (Show)

isReset :: Seq Int -> Int -> Bool
isReset (viewl -> EmptyL) _ = True
isReset (viewl -> x:<xs) i
  | x /= i = False
  | otherwise = isReset xs (i + 1)

split :: Seq Int -> Hold -> Hold
split r (Hold q (viewl -> b:<bs)) = Hold (q >< bs) (b <| r)

tick :: Int -> Hold -> Hold
tick 0 h = h
tick i (Hold (viewl -> b:<bs) r) = tick (i-1) $ Hold bs (b <| r)

tickFive :: Int -> Hold -> Hold
tickFive 0 h = h
tickFive i (Hold q r) = tickFive (i - 1) $ split r $ tick 5 $ Hold q empty

tickHour :: Int -> Hold -> Hold
tickHour 0 hold = hold
tickHour i (Hold q r) = tickHour (i - 1) $ split r $ tickFive 12 $ Hold q empty

tickHalfDay :: Seq Int -> Seq Int
tickHalfDay q = (\ (Hold q (viewl -> b:<bs)) -> q >< bs >< singleton b) $ tickHour 12 $ Hold q empty

go :: Int -> Seq Int -> Int
go i q
  | isReset q 1 && i /= 0 = i
  | otherwise = go (i + 1) (tickHalfDay (tickHalfDay q))

main = print $ go 0 $ fromList [1..123]
