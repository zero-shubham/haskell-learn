avg :: (Foldable t) => t Int -> Int
avg ns = sum ns `div` length ns

double :: (Num a) => a -> a
double x = 2 * x

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

lastEle :: [a] -> a
lastEle ns = head (reverse ns)

safeTail1 :: [a] -> [a]
safeTail1 [] = []
safeTail1 (_ : xs) = xs

safeTail2 :: [a] -> [a]
safeTail2 xs
  | null xs = []
  | otherwise = tail xs

safeTail3 :: [a] -> [a]
safeTail3 xs =
  if null xs
    then []
    else tail xs

pyths :: (Num c, Enum c, Eq c) => c -> [(c, c, c)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [x .. n], z <- [y .. n], x * x + y * y == z * z]

factors :: (Integral a) => a -> [a]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfect x = sum (init (factors x)) == x

perfects n = [x | x <- [1 .. n], perfect x]

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) =
  if x <= y
    then
      x : merge xs (y : ys)
    else
      y : merge (x : xs) ys

halves :: [a] -> ([a], [a])
halves xs = splitAt (div (length xs) 2) xs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge ys zs
  where
    (ys, zs) = halves xs