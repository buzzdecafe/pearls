import Data.Array
import Data.List

-- naive array-based solution 
-- this example cribbed from derekmcloughlin/pearls to get started

-- naive minfee is \Theta(n^2)
minfree1 :: [Integer] -> Integer
minfree1 xs = head([0..] \\ xs)

-- using Data.List \\ definition here:
--(\\) :: Eq a => [a] -> [a] -> [a]
--us \\ vs = filter (notElem' vs) us

notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = notElem n a


-- find the first False solution
-- iterates a list of Bools until it encounters a False, and returns the length of 
-- `True`s it has accumulated. The length of that list is the smallest free value.
minfree2 :: [Int] -> Int
minfree2 = search . checklist

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True)) 
    where n = length xs

search :: Array Int Bool -> Int
search = length . takeWhile id . elems


-- divide and conquer solution
minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs) | n == 0       = a
                  | m == b - a   = minfrom b (n - m, vs)
                  | otherwise    = minfrom a (m, us)
                  where (us, vs) = partition (< b) xs
                        b        = a + 1 + n `div` 2
                        m        = length us

minfree3 :: [Int] -> Int
minfree3 xs = minfrom 0 (length xs, xs)

-- [8, 23,  9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6]
-- minfree is `15`

