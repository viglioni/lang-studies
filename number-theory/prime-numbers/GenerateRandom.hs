-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:
-- Generates an list with n random integers in the interval [min,max]

-- 🇧🇷
-- Gera uma lista com n inteiros aleatórios no intervalo [min,max]

-- gen_random_arr
-- @param min (integer)
-- @param max (integer)
-- @param seed (seed)
-- @param n (integer)
-- @return [a1,a2,...,an] (arr of integers)

module GenerateRandom where

import System.Random

gen_random_arr :: (Integral a, Random a, RandomGen g) => a -> a -> g ->  a ->  [a]
gen_random_arr min max seed n = 
   map (fst) (foldl (\ (a:cc) _ -> (randomR (min,max) $ snd a):(a:cc)) [ randomR (min,max) seed ] [1..n])
