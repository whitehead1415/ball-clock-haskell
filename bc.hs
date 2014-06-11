import Prelude hiding (reverse, splitAt, take, drop)
import Data.Sequence

isSeq :: Seq Int -> Int -> Bool
isSeq (viewl -> EmptyL) = True
isSeq (viewl -> x :< xs) i = (x == i+1) && (isSeq (xs) (i + 1))

tickHours :: Seq Int -> Seq Int -> Int -> Seq Int
tickHours q hours 12 = (\ q -> (drop 1 q) >< hours >< (take 1 q)) $ tickFives q empty 1
tickHours q hours count = (\ q -> tickHours (drop 1 q) ((index q 0)<|hours) (count + 1)) $ tickFives q empty 1

tickFives :: Seq Int -> Seq Int -> Int -> Seq Int
tickFives q fives 12 = tickOnes q >< fives
tickFives q fives count = (\ q -> tickFives (drop 1 q) ((index q 0)<|fives) (count + 1)) $ tickOnes q

tickOnes :: Seq Int -> Seq Int
tickOnes q = (\ (x, y) -> y >< reverse x) $ splitAt 4 q

--getDays :: Int -> Seq Int -> Int
--getDays halfDays q = if halfDays > 1 && isSeq q then quot halfDays 2
                     --else getDays (halfDays + 1) $ tickHours q empty 1

--main = print $ getDays 1 $ fromList [1..30]

