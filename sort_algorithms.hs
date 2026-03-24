{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

list :: [Integer]
list = [1,5,2,3,1,4,6,1,2]
fastSort :: Ord a => [a] -> [a]
fastSort [] = []
fastSort (x:xs) = fastSort [ y | y <- xs, y <= x ] ++ [x] ++ fastSort [ y | y <- xs, y > x ]


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x<=y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let n = length xs `div` 2
        (left, right) = splitAt n xs
    in  merge (mergeSort left) (mergeSort right)

insert :: Ord t => t -> [t] -> [t]
insert n (x:xs)
    | n>x =x: insert n xs
    | otherwise = n:x:xs
insert n [] = [n]
insertSort :: Ord a => [a] -> [a]
insertSort  = foldr insert []


main :: IO ()
main = do
    
    print "FastSort:"
    print (fastSort list)

    print "MergeSort:"
    print (mergeSort list)

    print "InsertSort:"
    print (insertSort list)
