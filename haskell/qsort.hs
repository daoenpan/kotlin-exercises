-- Quick sort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [a | a <- xs, a > x]

-- How should the definition of the function qsort be modified so that it
-- produces a reverse sorted version of a list?

rqsort :: (Ord a) => [a] -> [a]
rqsort [] = []
rqsort (x : xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [a | a <- xs, a > x]

-- qsort removing duplicate elements
unique_sort :: Ord a => [a] -> [a]
unique_sort [] = []
unique_sort (x : xs) =
  let smaller = [a | a <- xs, a < x]
      larger  = [a | a <- xs, a > x]
   in unique_sort smaller ++ [x] ++ unique_sort larger
