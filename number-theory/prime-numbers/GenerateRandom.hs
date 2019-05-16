-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:

-- 🇧🇷


module GenerateRandom where

import System.Random

gen_random_arr :: (Integral a, Random a, RandomGen g) => a -> a -> g ->  a ->  [a]
gen_random_arr min max seed n = 
   map (fst) (foldl (\ (a:cc) _ -> (randomR (min,max) $ snd a):(a:cc)) [ randomR (min,max) seed ] [1..n])
