-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:
-- Algorithm to generate randomly a prime number
-- prime_generator a b -> generates a prime p such that a < p < b.
-- prime_gen_by_bits a -> generates a prime p such that 2^a < p < 2^(a+1), i.e. p has a bits.
-- BE CAREFUL! This algorithm may never stop! If there is no prime in range (a,b) it will run forever.
-- BE CAREFUL![2] The prime generated is tested with two probabilistic algorithms (fermat 50 times and  miller-rabin, also 50 times) but there is a slightly chance that the number is not a prime. 

-- 🇧🇷
-- Algoritmo para gerar aleatoriamente um número primo
-- prime_generator a b -> gera um primo p tal que a<p<b
-- prime_gen_by_bits a -> gera um primo p tal que 2^a < p < 2^(a+1), i.e. p tem a bits.
-- ATENÇÃO! Este algoritmo pode não parar! Se não houver nenhum primo no intervalo (a,b) o algoritmo irá rodar para sempre.
-- ATENÇÃO![2] O primo gerado é testado usando dois algoritmos probabilísticos (fermat 50 vezes e miller-rabin outras 50), mas existe uma pequena chance de o número gerado não ser primo.

-- prime_gen
-- @param a (integer)
-- @param b (integer)
-- @return p (integer)

-- prime_gen_by_bits
-- @param a (integer)
-- @return p (integer)


module PrimeGenerator where

import System.Random
import Fermat
import MillerRabin

prime_gen_by_bits :: (Integral t, Random t) => t -> IO t
prime_gen_by_bits bits = prime_gen (2^bits) (2^(bits+1))

prime_gen :: (Integral a, Random a) => a -> a -> IO a
prime_gen min max = do
  seed <- newStdGen
  return $ prime_gen_pure min max seed

prime_gen_pure :: (Integral t, RandomGen g, Random t) => t -> t -> g -> t
prime_gen_pure min max seed 
  | is_prime = number
  | otherwise = prime_gen_pure min max new_seed
  where
    (number, new_seed) = randomR (min, max) seed
    random_arr1 = gen_random_arr 2 (number-1) new_seed 50
    random_arr2 = gen_random_arr 2 (number-1) (snd $ next new_seed) 50
    fermat = Fermat.test_pure number random_arr1
    miller_rabin = MillerRabin.test_pure number random_arr2
    is_prime = fermat && miller_rabin

gen_random_arr :: (Integral a, Random a, RandomGen g) => a -> a -> g ->  a ->  [a]
gen_random_arr min max seed n = 
   map (fst) (foldl (\ (a:cc) _ -> (randomR (min,max) $ snd a):(a:cc)) [ randomR (min,max) seed ] [1..n])




