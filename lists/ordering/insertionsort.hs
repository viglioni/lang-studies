-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- 🇬🇧
-- Insertionsort algorithm
-- 🇧🇷
-- Algoritmo de ordenação insertionsort

-- @param  arr         (array of ordenable itens)
-- @return ordered_arr (array of ordenable itens)
insertionsort :: (Ord a) => [a] -> [a]
insertionsort array = foldl insert [] array

-- :gb: Insert element in array
-- :br: Insere elemento no vetor
insert :: (Ord a) => [a] -> a -> [a]
insert arr element = smaller ++ (element:greater)  
  where
    smaller = [x | x<-arr, x <= element ]
    greater = [x | x<-arr, x >  element ]


