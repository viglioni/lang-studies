-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:
-- Fermat's algorithm to test primality
-- This is a probabilistic algorithm
-- False means it is NOT a prime
-- True means it probably is prime
-- First parameter is a integer to be tested
-- The second is how many times it must be tested

-- 🇧🇷
-- Algoritmo de Fermat para testar primalidade
-- É um algoritmo probabilístico
-- False significa que o número NÃO é primo
-- True significa que o número provavelmente é primo
-- O primeiro parâmetro é um inteiro a ser testado
-- O segundo é quantas vezes ele deve ser testado

-- @param prime (integer)
-- @param repeat (integer)
-- @return d,s,t (integer, integer, integer)

module FERMAT where

import System.Random
import MODULAREXP

fermat_test :: (Integral t, Random t) => t -> t -> IO Bool
fermat_test prime repeat =
  do
    seed <- getStdGen
    return $ fermat_aux prime repeat seed 1 True


fermat_aux :: (Integral t, RandomGen g, Random t) => t -> t -> g -> t -> Bool -> Bool
fermat_aux 2 _ _ _ _ = True
fermat_aux prime repeat seed counter previous =
  if prime < 2 then False
  else
    if counter <= repeat && previous 
    then fermat_aux prime repeat new_seed (counter+1) test
    else previous
  where
    (a,new_seed) = randomR (2, (prime-1)) seed
    test = test_prime prime a

test_prime :: (Integral a, Random a) => a -> a -> Bool
test_prime prime a
  | mod_exp a (prime-1) prime == 1 = True
  | otherwise = False
