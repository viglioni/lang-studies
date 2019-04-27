-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- 🇬🇧
-- Mergesort algorithm
-- 🇧🇷
-- Algoritmo de ordenação mergesort

-- @param  arr         (array of ordenable itens)
-- @return ordered_arr (array of ordenable itens)
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort (a:[]) = [a]
mergesort array = merge left_sorted right_sorted
  where
    half = div (length array) 2
    (left_array, right_array) = splitAt half array
    left_sorted = mergesort left_array
    right_sorted = mergesort right_array


-- :gb: merge two ordered arrays
-- :br: mescla dois vetores ordenados
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (x:xs) (y:ys)
  | x < y = x:(merge xs (y:ys))
  | otherwise = y:(merge (x:xs) ys)


