-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- 🇬🇧
-- Bubblesort algorithm
-- 🇧🇷
-- Algoritmo de ordenação bubblesort

-- @param  arr         (array of ordenable itens)
-- @return ordered_arr (array of ordenable itens)
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort (a:[]) = [a]
bubbleSort a = (iterate bubble a) !! (length a)
  where
    bubble :: (Ord a) => [a] -> [a]
    bubble (a:b:[]) 
      | a < b = a:b:[]
      | otherwise = b:a:[]
    bubble (a:b:bs) 
      | a < b = a:(bubble (b:bs))
      | otherwise = b:(bubble (a:bs))
