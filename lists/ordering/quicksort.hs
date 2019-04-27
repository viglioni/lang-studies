-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- 🇬🇧
-- Quicksort algorithm
-- 🇧🇷
-- Algoritmo de ordenação quicksort

-- @param  arr         (array of ordenable itens)
-- @return ordered_arr (array of ordenable itens)
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [a] = [a]
quicksort array = sorted_smaller ++ (pivot:sorted_greater)
  where
    half = div (length array) 2
    pivot = array !! half
    (left_arr,right_arr) = splitAt half array
    array_without_pivot = left_arr ++ (tail right_arr)
    smaller_elements = [x | x<- array_without_pivot, x <= pivot]
    greater_elements = [x | x<- array_without_pivot, x > pivot]
    sorted_smaller = quicksort smaller_elements
    sorted_greater = quicksort greater_elements


