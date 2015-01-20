import Data.Array

-- naive array-based solution 
-- this example cribbed from derekmcloughlin/pearls to get started

-- naive minfee is \Theta(n^2)
minfree1 :: [Integer] -> Integer
minfree1 xs = head([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

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
