import Data.Sequence

data Hold = Hold { q :: Row, row :: Row} deriving (Show)

type Row = Seq Int

startList :: Row
startList = fromList [1..30]

isDone :: Int -> Bool
isDone x = x == 108855

isReset :: Row -> Int -> Bool
isReset (viewl -> EmptyL) _ = True
isReset (viewl -> x:<xs) i
  | x /= i = False
  | otherwise = isReset xs (i + 1)

split :: Row -> Hold -> Hold
split r (Hold q (viewl -> b:<bs)) = Hold (q >< bs) (b <| r)

nTimes :: Int -> (Hold -> Hold) -> Hold -> Hold
nTimes 0 _ x = x
nTimes i f x = nTimes (i - 1) f (f x)

deque :: Hold -> Hold
deque (Hold (viewl -> b:<bs) r) = Hold bs (b <| r)

tick :: Int -> Hold -> Hold
tick i h = nTimes i deque h

tickFive :: Int -> Hold -> Hold
tickFive 0 h = h
tickFive i (Hold q r) = tickFive (i - 1) $ split r $ tick 5 $ Hold q empty

tickHour :: Int -> Hold -> Hold
tickHour 0 hold = hold
tickHour i (Hold q r) = tickHour (i - 1) $ split r $ tickFive 12 $ Hold q empty

tickHalfDay :: Row -> Row
tickHalfDay q = (\ (Hold q (viewl -> b:<bs)) -> q >< bs >< singleton b) $ tickHour 12 $ Hold q empty

run :: Int -> Row -> Int
run i q
  | isReset q 1 && i /= 0 = i
  | otherwise = run (i + 1) (tickHalfDay (tickHalfDay q))

main = print $ run 0 startList
