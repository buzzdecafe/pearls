
-- msc: Maximum Surpasser Count
msc :: Ord a => [a] -> Int
msc xs = maximum[scount z zs | z : zs <- tails xs]

-- `scount x xs` is the surpasser count of x in xs
scount x xs = length (filter (x <) xs)

-- tails returns the nonempty tails of nonempty list in order of decreasing length
tails [] = []
tails (x:xs) = (x:xs) : tails xs

-- main = print $ msc "GENERATING"

-- hey, that works! but it's O(n^2)


-- divide and conquer approach:
msc2 :: Ord a => [a] -> Int
msc2 xs = maximum . map snd $ table xs

-- need to find a function `table` that indexes surpasser counts
--and then a function `join` s.t. `table (xs ++ ys) = join (table xs) (table ys)
table [x] = [(x, 0)]
table xs = join (m - n) (table ys) (table zs)
           where m        = length xs
                 n        = m `div` 2
                 (ys, zs) = splitAt n xs

join 0 txs [] = txs
join n [] tys = tys
join n txs@((x, c): txs') tys@((y, d): tys')
              | x < y  = (x, c + n) : join n txs' tys
              | x >= y = (y, d) : join (n - 1) txs tys'


main = print $ msc2 "GENERATING"
