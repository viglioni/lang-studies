-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:
-- Algorithm to generate randomly a prime number
-- prime_generator a b -> generates a prime p such that a < p < b.
-- prime_gen_by_bits a -> generates a prime p such that 2^a < p < 2^(a+1), i.e. p has a bits.
-- BE CAREFUL! This algorithm may never stop! If there is no prime in range (a,b) it will run forever.
-- BE CAREFUL![2] The prime generated is tested with a probabilistic algorithm (fermat) 100 times, but there is a slightly chance that the number is not a prime. 

-- 🇧🇷
-- Algoritmo para gerar aleatoriamente um número primo
-- prime_generator a b -> gera um primo p tal que a<p<b
-- prime_gen_by_bits a -> gera um primo p tal que 2^a < p < 2^(a+1), i.e. p tem a bits.
-- ATENÇÃO! Este algoritmo pode não parar! Se não houver nenhum primo no intervalo (a,b) o algoritmo irá rodar para sempre.
-- ATENÇÃO![2] O primo gerado é testado usando um algoritmo probabilístico (fermat) 100 vezes, mas existe uma pequena chance de o número gerado não ser primo.

-- prime_generator
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

-- prime_gen_by_bits :: (Integral t, Random t) => t -> IO t
-- prime_gen_by_bits bits = prime_generator (2^bits) (2^(bits+1))

-- prime_generator :: (Integral t, Random t) => t -> t -> IO t
-- prime_generator min max =
--   do
--     seed <- newStdGen
--     return $ pr_gen min max seed

pr_gen :: (Integral t, Random t, RandomGen g) => t -> t -> g ->  IO t
pr_gen min max seed =
  do
    is_prime <- fermat_test generated 50
    if is_prime then return generated else return $ pr_gen min max new_seed
      where
        (generated, new_seed) = randomR (min,max) seed
