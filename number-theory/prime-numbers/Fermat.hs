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

-- fermat_test
-- @param prime (integer)
-- @param repeat (integer)
-- @return is_prime (Bool)

module Fermat where

import System.Random
import ModularExp
<<<<<<< HEAD:number-theory/prime-numbers/Fermat.hs
=======
import GenerateRandom
>>>>>>> bugs:number-theory/prime-numbers/Fermat.hs

fermat_test :: (Integral t, Random t) => t -> t -> IO Bool
fermat_test prime repeat =
  do
    seed <- newStdGen
<<<<<<< HEAD:number-theory/prime-numbers/Fermat.hs
=======
    let arr = gen_random_arr 2 (prime-1) (snd $ next seed) repeat
    return $ test_pure prime arr


test_pure :: (Integral t, Random t) => t -> [t] -> Bool
test_pure prime arr = is_prime
  where
    booleans = map (\a -> unitary_test prime a) arr
    is_prime = foldl (&&) True booleans
>>>>>>> bugs:number-theory/prime-numbers/Fermat.hs


unitary_test :: (Integral a, Random a) => a -> a -> Bool
unitary_test prime a
  | mod_exp a (prime-1) prime == 1 = True
  | otherwise = False

